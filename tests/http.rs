//! Talking to a `std::http` server over real sockets, byte by byte, as an
//! HTTP client would.

use std::{
    io::{BufRead, BufReader, Read, Write},
    net::TcpStream,
    process::{Child, Command, Stdio},
    thread::sleep,
    time::Duration,
};

/// A server that echoes what it was asked, with a short timeout and a
/// small body limit.
const SERVER: &str = r#"import std::http::{Request, Response, Options};
import std::http;
import std::net;
import std::time::{Duration};

fn main() {
    let listener = match net::listen("127.0.0.1:0") { Ok(l) => l, Err(e) => { println(e.message); return; } };
    println(listener.local_addr());
    let options = Options { timeout: Some(Duration::from_millis(300)), max_body: 100 };
    http::serve_with(listener, options, fn(req: Request) -> Response {
        let q = req.query.get("q").unwrap_or("-");
        Response::text(200, "{req.method} {req.path} q={q} body={req.body}")
            .with_header("Set-Cookie", "a=1")
            .with_header("Set-Cookie", "b=2")
    });
}
"#;

/// The server, killed when dropped.
struct Server(Child);

impl Drop for Server {
    fn drop(&mut self) {
        self.0.kill().ok();
        self.0.wait().ok();
    }
}

/// Start the server, and give it with the address it listens at.
fn start(name: &str) -> (Server, String) {
    let dir = std::env::temp_dir().join(format!("aja-http-{name}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("server.aja");
    std::fs::write(&file, SERVER).unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_aja"))
        .arg(&file)
        .env("NO_COLOR", "1")
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .expect("aja runs");
    let mut addr = String::new();
    BufReader::new(child.stdout.take().unwrap())
        .read_line(&mut addr)
        .unwrap();
    std::fs::remove_dir_all(&dir).ok();
    (Server(child), addr.trim().to_string())
}

fn connect(addr: &str) -> BufReader<TcpStream> {
    let s = TcpStream::connect(addr).unwrap();
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    BufReader::new(s)
}

fn send(conn: &mut BufReader<TcpStream>, bytes: &[u8]) {
    conn.get_mut().write_all(bytes).unwrap();
}

/// The next response: its head, as sent, and its body.
fn response(conn: &mut BufReader<TcpStream>) -> (String, String) {
    let mut head = String::new();
    let mut length = 0;
    loop {
        let mut line = String::new();
        assert!(
            conn.read_line(&mut line).unwrap() > 0,
            "the connection closed"
        );
        if let Some(n) = line.to_lowercase().strip_prefix("content-length: ") {
            length = n.trim().parse().unwrap();
        }
        head.push_str(&line);
        if line == "\r\n" {
            break;
        }
    }
    let mut body = vec![0; length];
    conn.read_exact(&mut body).unwrap();
    (head, String::from_utf8(body).unwrap())
}

/// Whether the server has closed the connection.
fn closed(conn: &mut BufReader<TcpStream>) -> bool {
    let mut rest = Vec::new();
    conn.read_to_end(&mut rest).is_ok() && rest.is_empty()
}

#[test]
fn requests_share_a_connection_until_it_closes() {
    let (_server, addr) = start("keepalive");
    let mut c = connect(&addr);
    // Two requests sent at once (pipelined), then a third that asks to close.
    send(
        &mut c,
        b"GET /a?q=x%20y HTTP/1.1\r\nHost: t\r\n\r\nPOST /b HTTP/1.1\r\nHost: t\r\nContent-Length: 5\r\n\r\nhello",
    );
    let (head, body) = response(&mut c);
    assert!(head.starts_with("HTTP/1.1 200 OK\r\n"), "{head}");
    assert!(head.contains("Connection: keep-alive\r\n"), "{head}");
    assert!(
        head.contains("Set-Cookie: a=1\r\nSet-Cookie: b=2\r\n"),
        "{head}"
    );
    assert_eq!(body, "GET /a q=x y body=");
    assert_eq!(response(&mut c).1, "POST /b q=- body=hello");
    send(
        &mut c,
        b"GET /c HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n",
    );
    let (head, body) = response(&mut c);
    assert!(head.contains("Connection: close\r\n"), "{head}");
    assert_eq!(body, "GET /c q=- body=");
    assert!(closed(&mut c));
}

#[test]
fn chunked_bodies_can_split_a_character() {
    let (_server, addr) = start("chunked");
    let mut c = connect(&addr);
    // "é" is C3 A9, here in two chunks, with an extension and a trailer.
    send(
        &mut c,
        b"POST / HTTP/1.1\r\nHost: t\r\nTransfer-Encoding: chunked\r\n\r\n\
          4;x=1\r\ncaf\xc3\r\n1\r\n\xa9\r\n0\r\nX-Trailer: 1\r\n\r\n",
    );
    assert_eq!(response(&mut c).1, "POST / q=- body=café");
}

#[test]
fn a_request_arriving_in_pieces_is_put_together() {
    let (_server, addr) = start("pieces");
    let mut c = connect(&addr);
    for piece in [&b"GET /sl"[..], b"ow HTTP/1.1\r\nHo", b"st: t\r\n", b"\r\n"] {
        send(&mut c, piece);
        sleep(Duration::from_millis(20));
    }
    assert_eq!(response(&mut c).1, "GET /slow q=- body=");
}

#[test]
fn head_gets_no_body_and_100_continue_is_answered() {
    let (_server, addr) = start("head");
    let mut c = connect(&addr);
    send(&mut c, b"HEAD /x HTTP/1.1\r\nHost: t\r\n\r\n");
    let mut head = String::new();
    loop {
        let mut line = String::new();
        c.read_line(&mut line).unwrap();
        head.push_str(&line);
        if line == "\r\n" {
            break;
        }
    }
    assert!(head.contains("Content-Length: 17\r\n"), "{head}");
    // No body follows: the next thing on the connection is the next response.
    send(
        &mut c,
        b"POST /y HTTP/1.1\r\nHost: t\r\nExpect: 100-continue\r\nContent-Length: 2\r\n\r\n",
    );
    let mut line = String::new();
    c.read_line(&mut line).unwrap();
    assert_eq!(line, "HTTP/1.1 100 Continue\r\n");
    c.read_line(&mut line).unwrap();
    send(&mut c, b"ok");
    assert_eq!(response(&mut c).1, "POST /y q=- body=ok");
}

#[test]
fn bad_requests_are_answered_and_closed() {
    let (_server, addr) = start("bad");
    for (request, status) in [
        (&b"nonsense\r\n\r\n"[..], "400 Bad Request"),
        (b"GET / HTTP/1.1\r\n\r\n", "400 Bad Request"), // no Host
        (
            b"GET / HTTP/3\r\nHost: t\r\n\r\n",
            "505 HTTP Version Not Supported",
        ),
        (
            b"GET / HTTP/1.1\r\nHost: t\r\nbroken\r\n\r\n",
            "400 Bad Request",
        ),
        (
            b"POST / HTTP/1.1\r\nHost: t\r\nContent-Length: 101\r\n\r\n",
            "413 Content Too Large",
        ),
        (
            b"POST / HTTP/1.1\r\nHost: t\r\nTransfer-Encoding: gzip\r\n\r\n",
            "501 Not Implemented",
        ),
        (b"GET /\xff HTTP/1.1\r\nHost: t\r\n\r\n", "400 Bad Request"),
        // A body that stops short times out.
        (
            b"POST / HTTP/1.1\r\nHost: t\r\nContent-Length: 10\r\n\r\nabc",
            "408 Request Timeout",
        ),
    ] {
        let mut c = connect(&addr);
        send(&mut c, request);
        let (head, _) = response(&mut c);
        assert!(
            head.starts_with(&format!("HTTP/1.1 {status}\r\n")),
            "{}: {head}",
            String::from_utf8_lossy(request)
        );
        assert!(head.contains("Connection: close\r\n"), "{head}");
        assert!(closed(&mut c));
    }
}

#[test]
fn an_idle_connection_is_closed_quietly() {
    let (_server, addr) = start("idle");
    let mut c = connect(&addr);
    // Past the 300ms timeout, with nothing sent: no 408, just the end.
    assert!(closed(&mut c));
}
