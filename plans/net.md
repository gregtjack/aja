# Plan: `std::net`

Networking, starting with TCP. The green-threads plan
([green-threads.md](green-threads.md)) says a kqueue/epoll poller comes with
`std::net`; this is that plan. Its goal: a server that handles many
connections at once, each in a task of its own, written as straight-line
blocking code, while every task runs on one OS thread.

## Decisions

| Question | Decision |
|---|---|
| Blocking | **A socket operation blocks only its task.** Sockets are non-blocking underneath. An operation that would block registers what it waits for with a poller and parks the task; the call runs again once the socket is ready, like `join` and `sleep`. There is no `async`. |
| The poller | **The `polling` crate** (epoll, kqueue, IOCP), in one-shot mode: a readiness event fires once, then has to be asked for again, which matches "run the call again". It belongs to the scheduler, which polls without waiting at the end of each turn while any task waits on a socket, and waits in the poller, rather than sleeping, when no task can run. |
| What can't be non-blocking | **Runs on a thread of its own.** Resolving a name and connecting (std has no non-blocking connect) run on a short-lived thread, which wakes the poller with `notify` when done. This is the first piece of step 6 of the green-threads plan (blocking intrinsics off-thread), and the pool that step adds replaces the threads. |
| Data | **Text, for now.** Streams read and write `str`: `read_line`, `lines`, `read_to_string`, `write`. Reads are buffered on the Rust side, so a line split across packets, or a character split across reads, comes out whole; text that isn't UTF-8 is an `InvalidData` error. Bytes wait for a `Bytes` type. |
| Handles | **Explicit `close`, as an `int` in a struct.** `TcpListener` and `TcpStream` are structs holding a private handle into the VM's socket table, so the intrinsics stay scalar. Handles are never reused, so a stale one fails (`NotConnected`) rather than reaching another socket. Everything left open closes when the program ends. Closing on collection (a native heap object with a finalizer) can come later. |
| Errors | **`std::io::Error`**, with new kinds: `ConnectionRefused`, `ConnectionReset`, `AddrInUse`, `AddrNotAvailable`, and `NotConnected`. Messages name the address: `could not connect to '127.0.0.1:1': connection refused`. |
| Addresses | **Strings**: `"127.0.0.1:8080"`, `"localhost:80"`, `"[::1]:0"`. Port 0 picks a free port, which `local_addr()` reports. An `Addr` type can come with UDP. |

## The surface

```aja
import std::io;
import std::net;
import std::task;

fn main() -> Result<void, io::Error> {
    let listener = net::listen("127.0.0.1:7000")?;
    println("listening on {listener.local_addr()}");
    task::scope(fn(s) {
        while true {
            match listener.accept() {
                Ok(conn) => { s.spawn(fn() { echo(conn) }); }
                Err(e) => println("accept failed: {e.message}"),
            }
        }
    });
    Ok(void)
}

fn echo(conn: net::TcpStream) -> Result<void, io::Error> {
    defer conn.close();
    for line in conn.lines() {
        conn.write_line(line?)?;
    }
    Ok(void)
}
```

| `std::net` | Does |
|---|---|
| `listen(addr)` | `Result<TcpListener, io::Error>` |
| `connect(addr)` | `Result<TcpStream, io::Error>` |
| `listener.accept()` | `Result<TcpStream, io::Error>`, waiting for a connection |
| `listener.local_addr()`, `listener.close()` | |
| `stream.read_line()`, `stream.lines()`, `stream.read_to_string()` | As on `io::Reader` |
| `stream.write(text)`, `stream.write_line(text)` | `Result<void, io::Error>`, returning once all of it is written |
| `stream.close_write()` | Tell the other end nothing more is coming (a half close), so a peer reading to the end can finish |
| `stream.local_addr()`, `stream.peer_addr()`, `stream.close()` | |

## Runtime

- **`src/vm/net.rs`** holds `Net`: the poller (made when the first socket
  is), the socket table, each socket's read buffer and waiting tasks, and
  the pending writes and connects, by task.
- **Intrinsics.** The net functions are scheduler intrinsics, since they can
  wait. Each is declared in `std::native` (`tcp_accept(id: int) -> int`,
  ...) and takes and returns only scalars and `str`, as other intrinsics
  do.
- **Waiting.** An operation that gets `WouldBlock` adds the task to the
  socket's readers or writers, re-arms the socket's one-shot interest, and
  returns `Outcome::Wait`. When the poller reports the socket ready, the
  scheduler wakes those tasks, whose calls run again. Reads, accepts, and
  addresses have no side effects before they finish, so running again is
  safe. Writes and connects do, so they keep their progress in `pending`,
  by task: a write copies its text once and then sends what it can each
  time it runs; a connect starts its thread once and then checks for the
  result.
- **Deadlock.** A task waiting on a socket isn't deadlocked: something
  outside the program may wake it. As with sleepers, the scheduler waits in
  the poller instead of reporting a deadlock.

## Steps

1. **TCP.** The poller in the scheduler, `listen`, `accept`, `connect`,
   reading lines and to the end, writing, half and full close, addresses,
   and the new error kinds. An echo-server example, VM tests with switching
   and collection stress (a server and clients in tasks of one program),
   and an integration test that talks to `aja` over a real socket. ✅
   The poller is the scheduler's: it polls without waiting at the end of a
   turn while any task waits on the network, and waits in the poller, up to
   the next timer, when no task can run. Closing a socket wakes the tasks
   waiting on it, whose calls then fail with `NotConnected`. Loopback
   buffers hold megabytes, so the test that a write waits for its reader
   sends eight. Found on the way: there was no way to write a `void` value,
   so `Ok(...)` of a `Result<void, E>` had to wrap a call; the literal
   `void` came next.
2. **Timeouts.** `stream.set_timeout(d: Duration)` (with `std::time`),
   failing a read or write that waits longer with `TimedOut`. Timers and
   poller waits already live in one loop.
3. **UDP**, with `Addr` values for `send_to` and `recv_from`.
4. **Bytes**, once there is a `Bytes` type: `read(max)`, `read_exact(n)`,
   `write_bytes`.
5. **Closing on collection**, so a leaked stream doesn't stay open until the
   program ends.

## Not yet

TLS, HTTP (a library on top, later), Unix sockets, socket options beyond
timeouts, and `select` over several sockets (tasks and, later, channels
cover it).
