//! Talking to an `aja` server over real sockets, as a client would.

use std::{
    io::{BufRead, BufReader, Read, Write},
    net::TcpStream,
    process::{Child, Command, Stdio},
    thread::sleep,
    time::Duration,
};

/// An echo server, with a task per connection, that prints its address.
const SERVER: &str = "import std::io;
import std::net;
import std::task;

fn echo(conn: net::TcpStream) -> Result<void, io::Error> {
    for line in conn.lines() {
        conn.write_line(line?)?;
    }
    conn.close();
    Ok(void)
}

fn main() -> Result<void, io::Error> {
    let listener = net::listen(\"127.0.0.1:0\")?;
    println(listener.local_addr());
    task::scope(fn(s) {
        while true {
            match listener.accept() {
                Ok(conn) => { s.spawn(fn() { echo(conn) }); }
                Err(e) => println(e.message),
            }
        }
    });
    Ok(void)
}
";

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
    let dir = std::env::temp_dir().join(format!("aja-net-{name}-{}", std::process::id()));
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

fn connect(addr: &str) -> TcpStream {
    let s = TcpStream::connect(addr).unwrap();
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    s
}

fn reply(conn: &mut TcpStream, len: usize) -> String {
    let mut buf = vec![0; len];
    conn.read_exact(&mut buf).unwrap();
    String::from_utf8(buf).unwrap()
}

#[test]
fn a_connection_waiting_doesnt_hold_up_another() {
    let (_server, addr) = start("waiting");
    let mut a = connect(&addr);
    let mut b = connect(&addr);
    // `a` has sent nothing, so its task waits to read; `b` is answered
    // all the same.
    b.write_all(b"from b\n").unwrap();
    assert_eq!(reply(&mut b, 7), "from b\n");
    a.write_all(b"from a\n").unwrap();
    assert_eq!(reply(&mut a, 7), "from a\n");
    // A client finishing ends only its own connection.
    drop(b);
    a.write_all(b"again\n").unwrap();
    assert_eq!(reply(&mut a, 6), "again\n");
}

#[test]
fn text_arriving_in_pieces_comes_out_whole() {
    let (_server, addr) = start("pieces");
    let mut c = connect(&addr);
    // "é" is two bytes; send them, and the line, a piece at a time.
    for piece in [&b"caf\xc3"[..], b"\xa9 au ", b"lait\r", b"\n"] {
        c.write_all(piece).unwrap();
        c.flush().unwrap();
        sleep(Duration::from_millis(30));
    }
    assert_eq!(reply(&mut c, "café au lait\n".len()), "café au lait\n");
}
