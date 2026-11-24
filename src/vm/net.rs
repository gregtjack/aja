//! TCP for `std::net`: the socket table, and the poller that lets a socket
//! operation block only its task.
//!
//! Sockets are non-blocking. An operation that would block adds its task to
//! the socket's readers or writers, asks the poller to report the socket
//! ready (once: the poller is in one-shot mode), and returns
//! [`Outcome::Wait`]; the scheduler wakes the task when the poller reports
//! it, and the call runs again. Reads, accepts, and addresses do nothing
//! before they finish, so running again is safe. A write and a connect keep
//! their progress in [`Net::pending`], by task.
//!
//! Connecting, which std can't do without blocking, runs on a thread of its
//! own, which wakes the poller when it is done. See `plans/net.md`.

use std::{
    collections::HashMap,
    io::{self, Read, Write},
    net::{Shutdown, TcpListener, TcpStream},
    sync::{
        mpsc::{self, Receiver, Sender},
        Arc,
    },
    time::Duration,
};

use polling::{Event, Events, Poller};

use super::{
    heap::Heap,
    native::{record_as, set_status, str_value, END_OF_INPUT, NOT_CONNECTED, OK},
    scheduler::{Outcome, TaskId},
    value::Value,
};

/// The functions of `std::native` for TCP, which the runtime implements
/// itself since they can wait.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Listen,
    Accept,
    Connect,
    ReadLine,
    ReadAll,
    Read,
    Write,
    CloseWrite,
    Close,
    LocalAddr,
    PeerAddr,
}

impl Op {
    pub const ALL: [(Op, &'static str); 11] = [
        (Op::Listen, "std::native::tcp_listen"),
        (Op::Accept, "std::native::tcp_accept"),
        (Op::Connect, "std::native::tcp_connect"),
        (Op::ReadLine, "std::native::tcp_read_line"),
        (Op::ReadAll, "std::native::tcp_read_all"),
        (Op::Read, "std::native::tcp_read"),
        (Op::Write, "std::native::tcp_write"),
        (Op::CloseWrite, "std::native::tcp_close_write"),
        (Op::Close, "std::native::tcp_close"),
        (Op::LocalAddr, "std::native::tcp_local_addr"),
        (Op::PeerAddr, "std::native::tcp_peer_addr"),
    ];

    /// How many arguments it takes.
    pub fn arity(self) -> usize {
        match self {
            Op::Write | Op::Read => 2,
            _ => 1,
        }
    }
}

enum Kind {
    Listener(TcpListener),
    Stream {
        tcp: TcpStream,
        /// What has been read but not yet returned.
        input: Vec<u8>,
        /// Whether the other end has finished sending.
        eof: bool,
    },
}

struct Socket {
    kind: Kind,
    /// How it is named in messages: a listener's own address, a stream's
    /// peer's.
    name: String,
    /// The tasks waiting for it to be readable (or, for a listener, to have
    /// a connection), and writable.
    readers: Vec<TaskId>,
    writers: Vec<TaskId>,
}

impl Socket {
    fn interest(&self, id: i64) -> Event {
        Event::new(
            id as usize,
            !self.readers.is_empty(),
            !self.writers.is_empty(),
        )
    }
}

/// How much a read takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Until {
    Line,
    End,
    Bytes(usize),
}

/// A connect's task and ticket, and how it went.
type Connected = (TaskId, u64, io::Result<TcpStream>);

/// An operation that has made progress, and runs again to finish.
enum Pending {
    /// A write, with what it has sent so far.
    Write { data: Vec<u8>, written: usize },
    /// A connect, with its result once its thread has one.
    Connect {
        addr: String,
        /// Which connect this is, so a result that comes after its task
        /// was cancelled, and its slot went to another task, is ignored.
        ticket: u64,
        result: Option<io::Result<TcpStream>>,
    },
}

pub struct Net {
    /// Made with the first socket, so a program without any has none. In
    /// an `Arc` so a connect's thread can wake it.
    poller: Option<Arc<Poller>>,
    events: Events,
    sockets: HashMap<i64, Socket>,
    /// The handle the next socket gets. Handles are never reused, so a
    /// stale one fails instead of reaching another socket.
    next_id: i64,
    pending: HashMap<TaskId, Pending>,
    /// Finished connects, from their threads.
    connected: (Sender<Connected>, Receiver<Connected>),
    /// How many tasks are waiting, on a socket or a connect.
    waiting: usize,
    /// Tasks that an operation has woken, for the scheduler to queue.
    woken: Vec<TaskId>,
    /// How many connects have been started.
    tickets: u64,
}

impl Net {
    pub fn new() -> Self {
        Net {
            poller: None,
            events: Events::new(),
            sockets: HashMap::new(),
            next_id: 1,
            pending: HashMap::new(),
            connected: mpsc::channel(),
            waiting: 0,
            woken: Vec::new(),
            tickets: 0,
        }
    }

    /// Stop `task` waiting on anything here, since it has been cancelled,
    /// and drop what it had in progress.
    pub fn forget(&mut self, task: TaskId) {
        for socket in self.sockets.values_mut() {
            for list in [&mut socket.readers, &mut socket.writers] {
                let before = list.len();
                list.retain(|&t| t != task);
                self.waiting -= before - list.len();
            }
        }
        if let Some(Pending::Connect { result: None, .. }) = self.pending.remove(&task) {
            self.waiting -= 1;
        }
    }

    /// The tasks woken by the last operation, which can now run.
    pub fn take_woken(&mut self) -> Vec<TaskId> {
        std::mem::take(&mut self.woken)
    }

    /// Whether any task is waiting on the network, so the scheduler must
    /// poll rather than call it a deadlock.
    pub fn is_waiting(&self) -> bool {
        self.waiting > 0
    }

    /// Wait up to `timeout` (forever if `None`) for sockets to be ready, or
    /// connects to finish, and give the tasks that can now run.
    pub fn poll(&mut self, timeout: Option<Duration>) -> Vec<TaskId> {
        let mut woken = Vec::new();
        let Some(poller) = self.poller.clone() else {
            return woken;
        };
        self.events.clear();
        if let Err(e) = poller.wait(&mut self.events, timeout) {
            if e.kind() != io::ErrorKind::Interrupted {
                panic!("could not wait for sockets: {e}");
            }
        }
        for event in self.events.iter() {
            let id = event.key as i64;
            let Some(socket) = self.sockets.get_mut(&id) else {
                continue;
            };
            if event.readable {
                woken.append(&mut socket.readers);
            }
            if event.writable {
                woken.append(&mut socket.writers);
            }
            // The event used up the socket's interest; ask again for
            // whoever is still waiting.
            if !socket.readers.is_empty() || !socket.writers.is_empty() {
                let interest = socket.interest(id);
                rearm(&poller, socket, interest);
            }
        }
        while let Ok((task, ticket, result)) = self.connected.1.try_recv() {
            if let Some(Pending::Connect {
                ticket: t,
                result: slot,
                ..
            }) = self.pending.get_mut(&task)
            {
                if *t == ticket {
                    *slot = Some(result);
                    woken.push(task);
                }
            }
        }
        self.waiting -= woken.len();
        woken
    }

    /// Run `op` for `task` with `args`.
    pub fn call(&mut self, op: Op, task: TaskId, heap: &mut Heap, args: &[Value]) -> Outcome {
        let int = |i: usize| match args[i] {
            Value::Int(n) => n,
            _ => panic!("argument {i} of {op:?} should be an int"),
        };
        let text = |heap: &Heap, i: usize| match args[i] {
            Value::Str(s) => heap.str(s).to_string(),
            _ => panic!("argument {i} of {op:?} should be a str"),
        };
        let result = match op {
            Op::Listen => Done::Int(self.listen(&text(heap, 0))),
            Op::Connect => {
                let addr = text(heap, 0);
                match self.connect(task, addr) {
                    Some(id) => Done::Int(id),
                    None => return self.wait(task),
                }
            }
            Op::Accept => match self.accept(int(0)) {
                Step::Done(id) => Done::Int(id),
                Step::Wait => return self.wait_on(task, int(0), true),
            },
            Op::ReadLine | Op::ReadAll | Op::Read => {
                let until = match op {
                    Op::ReadLine => Until::Line,
                    Op::ReadAll => Until::End,
                    _ => Until::Bytes(int(1).max(0) as usize),
                };
                match self.read(int(0), until) {
                    Step::Done(text) => Done::Str(text),
                    Step::Wait => return self.wait_on(task, int(0), true),
                }
            }
            Op::Write => {
                let id = int(0);
                let data = || text(heap, 1).into_bytes();
                match self.write(task, id, data) {
                    Step::Done(()) => Done::Void,
                    Step::Wait => return self.wait_on(task, id, false),
                }
            }
            Op::CloseWrite => {
                self.close_write(int(0));
                Done::Void
            }
            Op::Close => {
                // The tasks waiting on it run again and find it gone.
                let mut woken = self.close(int(0));
                self.woken.append(&mut woken);
                Done::Void
            }
            Op::LocalAddr => Done::Str(self.addr(int(0), true)),
            Op::PeerAddr => Done::Str(self.addr(int(0), false)),
        };
        Outcome::Value(match result {
            Done::Int(n) => Value::Int(n),
            Done::Str(s) => str_value(heap, s),
            Done::Void => Value::Void,
        })
    }

    /// Make `task` wait for its pending connect, which `poll` sees finish.
    fn wait(&mut self, _task: TaskId) -> Outcome {
        self.waiting += 1;
        Outcome::Wait
    }

    /// Make `task` wait for socket `id` to be readable, or writable.
    fn wait_on(&mut self, task: TaskId, id: i64, read: bool) -> Outcome {
        let poller = self.poller.clone().expect("a socket has been made");
        let socket = self.sockets.get_mut(&id).expect("only a socket can block");
        if read {
            socket.readers.push(task);
        } else {
            socket.writers.push(task);
        }
        let interest = socket.interest(id);
        rearm(&poller, socket, interest);
        self.waiting += 1;
        Outcome::Wait
    }

    fn poller(&mut self) -> io::Result<Arc<Poller>> {
        if self.poller.is_none() {
            self.poller = Some(Arc::new(Poller::new()?));
        }
        Ok(self.poller.clone().unwrap())
    }

    /// Put a new socket in the table, giving its handle.
    fn add(&mut self, kind: Kind, name: String) -> io::Result<i64> {
        let poller = self.poller()?;
        let id = self.next_id;
        let source = match &kind {
            Kind::Listener(l) => {
                l.set_nonblocking(true)?;
                raw(l)
            }
            Kind::Stream { tcp, .. } => {
                tcp.set_nonblocking(true)?;
                raw(tcp)
            }
        };
        // Safety: the socket is deleted from the poller before it is
        // dropped, in `close` and when the `Net` is.
        unsafe { poller.add(source, Event::none(id as usize))? };
        self.next_id += 1;
        let socket = Socket {
            kind,
            name,
            readers: Vec::new(),
            writers: Vec::new(),
        };
        self.sockets.insert(id, socket);
        Ok(id)
    }

    fn listen(&mut self, addr: &str) -> i64 {
        let result = TcpListener::bind(addr).and_then(|l| {
            let name = l
                .local_addr()
                .map_or_else(|_| addr.to_string(), |a| a.to_string());
            self.add(Kind::Listener(l), name)
        });
        record_as(result, |e| format!("could not listen on '{addr}': {e}"))
    }

    /// Start connecting `task` to `addr` on a thread, or, once the thread
    /// has finished, give the new socket's handle (0 if it failed). `None`
    /// while it is connecting.
    fn connect(&mut self, task: TaskId, addr: String) -> Option<i64> {
        match self.pending.remove(&task) {
            Some(Pending::Connect {
                addr,
                result: Some(result),
                ..
            }) => {
                let result = result.and_then(|tcp| {
                    let name = tcp
                        .peer_addr()
                        .map_or_else(|_| addr.clone(), |a| a.to_string());
                    self.add(
                        Kind::Stream {
                            tcp,
                            input: Vec::new(),
                            eof: false,
                        },
                        name,
                    )
                });
                Some(record_as(result, |e| {
                    format!("could not connect to '{addr}': {e}")
                }))
            }
            Some(pending @ Pending::Connect { result: None, .. }) => {
                self.pending.insert(task, pending);
                None
            }
            Some(Pending::Write { .. }) => unreachable!("a task waits on one call at a time"),
            None => {
                let poller = match self.poller() {
                    Ok(p) => p,
                    Err(e) => {
                        return Some(record_as(Err(e), |e| {
                            format!("could not connect to '{addr}': {e}")
                        }))
                    }
                };
                let done = self.connected.0.clone();
                let to = addr.clone();
                self.tickets += 1;
                let ticket = self.tickets;
                std::thread::spawn(move || {
                    let result = TcpStream::connect(&to);
                    // The VM may have ended, and these with it.
                    let _ = done.send((task, ticket, result));
                    let _ = poller.notify();
                });
                self.pending.insert(
                    task,
                    Pending::Connect {
                        addr,
                        ticket,
                        result: None,
                    },
                );
                None
            }
        }
    }

    fn accept(&mut self, id: i64) -> Step<i64> {
        let Some(Socket {
            kind: Kind::Listener(listener),
            name,
            ..
        }) = self.sockets.get(&id)
        else {
            return Step::Done(not_connected("accept a connection on"));
        };
        let name = name.clone();
        let accepted = loop {
            match listener.accept() {
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => return Step::Wait,
                Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
                result => break result,
            }
        };
        let result = accepted.and_then(|(tcp, peer)| {
            let stream = Kind::Stream {
                tcp,
                input: Vec::new(),
                eof: false,
            };
            self.add(stream, peer.to_string())
        });
        Step::Done(record_as(result, |e| {
            format!("could not accept a connection on '{name}': {e}")
        }))
    }

    /// What `until` asks for from stream `id`: the next line, without its
    /// line ending; everything up to the end; or the next `n` bytes (fewer
    /// only if the stream ends first). Reading a line or to the end with
    /// nothing left sets the status to `END_OF_INPUT`; reading bytes there
    /// gives "".
    fn read(&mut self, id: i64, until: Until) -> Step<String> {
        let line = until == Until::Line;
        let Some(Socket {
            kind: Kind::Stream { tcp, input, eof },
            name,
            ..
        }) = self.sockets.get_mut(&id)
        else {
            return Step::Done(not_connected("read from"));
        };
        loop {
            if let Until::Bytes(n) = until {
                if input.len() >= n || *eof {
                    let rest = input.split_off(n.min(input.len()));
                    let taken = std::mem::replace(input, rest);
                    let result = String::from_utf8(taken).map_err(|_| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            "the text isn't UTF-8, or a character was cut in two",
                        )
                    });
                    return Step::Done(record_as(result, |e| {
                        format!("could not read from '{name}': {e}")
                    }));
                }
            }
            let found = if line {
                input.iter().position(|&b| b == b'\n')
            } else {
                None
            };
            if !matches!(until, Until::Bytes(_)) && (found.is_some() || *eof) {
                if input.is_empty() && found.is_none() {
                    set_status(END_OF_INPUT, String::new());
                    return Step::Done(String::new());
                }
                let mut bytes: Vec<u8> = match found {
                    Some(at) => {
                        let rest = input.split_off(at + 1);
                        let mut taken = std::mem::replace(input, rest);
                        taken.pop();
                        if taken.last() == Some(&b'\r') {
                            taken.pop();
                        }
                        taken
                    }
                    None => std::mem::take(input),
                };
                if line && found.is_none() && bytes.last() == Some(&b'\r') {
                    bytes.pop();
                }
                let result = String::from_utf8(bytes).map_err(|_| {
                    io::Error::new(io::ErrorKind::InvalidData, "the text isn't UTF-8")
                });
                return Step::Done(record_as(result, |e| {
                    format!("could not read from '{name}': {e}")
                }));
            }
            let mut buf = [0; 8192];
            match tcp.read(&mut buf) {
                Ok(0) => *eof = true,
                Ok(n) => input.extend_from_slice(&buf[..n]),
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => return Step::Wait,
                Err(e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => {
                    return Step::Done(record_as(Err(e), |e| {
                        format!("could not read from '{name}': {e}")
                    }))
                }
            }
        }
    }

    /// Send all of `data` on stream `id`. The first time it runs for a
    /// write, it takes the data; after that it carries on from where it got
    /// to.
    fn write(&mut self, task: TaskId, id: i64, data: impl FnOnce() -> Vec<u8>) -> Step<()> {
        let Some(Socket {
            kind: Kind::Stream { tcp, .. },
            name,
            ..
        }) = self.sockets.get_mut(&id)
        else {
            self.pending.remove(&task);
            not_connected::<()>("write to");
            return Step::Done(());
        };
        let Pending::Write { data, written } =
            self.pending.entry(task).or_insert_with(|| Pending::Write {
                data: data(),
                written: 0,
            })
        else {
            unreachable!("a task waits on one call at a time")
        };
        let result = loop {
            if *written == data.len() {
                break Ok(());
            }
            match tcp.write(&data[*written..]) {
                Ok(n) => *written += n,
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => return Step::Wait,
                Err(e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => break Err(e),
            }
        };
        let name = name.clone();
        self.pending.remove(&task);
        record_as(result, |e| format!("could not write to '{name}': {e}"));
        Step::Done(())
    }

    fn close_write(&mut self, id: i64) {
        let Some(Socket {
            kind: Kind::Stream { tcp, .. },
            name,
            ..
        }) = self.sockets.get(&id)
        else {
            return not_connected("close");
        };
        record_as(tcp.shutdown(Shutdown::Write), |e| {
            format!("could not close '{name}': {e}")
        })
    }

    /// Close socket `id`, giving the tasks that were waiting on it.
    fn close(&mut self, id: i64) -> Vec<TaskId> {
        set_status(OK, String::new());
        let Some(mut socket) = self.sockets.remove(&id) else {
            return Vec::new();
        };
        if let Some(poller) = &self.poller {
            let _ = match &socket.kind {
                Kind::Listener(l) => poller.delete(l),
                Kind::Stream { tcp, .. } => poller.delete(tcp),
            };
        }
        let mut woken = std::mem::take(&mut socket.readers);
        woken.append(&mut socket.writers);
        self.waiting -= woken.len();
        woken
    }

    /// Socket `id`'s own address, or its peer's.
    fn addr(&self, id: i64, local: bool) -> String {
        let Some(socket) = self.sockets.get(&id) else {
            return not_connected("find the address of");
        };
        let addr = match (&socket.kind, local) {
            (Kind::Listener(l), true) => l.local_addr(),
            (Kind::Stream { tcp, .. }, true) => tcp.local_addr(),
            (Kind::Stream { tcp, .. }, false) => tcp.peer_addr(),
            (Kind::Listener(_), false) => Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "a listener has no peer",
            )),
        };
        let name = &socket.name;
        record_as(addr.map(|a| a.to_string()), |e| {
            format!("could not find the address of '{name}': {e}")
        })
    }
}

impl Drop for Net {
    fn drop(&mut self) {
        let ids: Vec<i64> = self.sockets.keys().copied().collect();
        for id in ids {
            self.close(id);
        }
    }
}

/// How an operation that may block went.
enum Step<T> {
    Done(T),
    Wait,
}

enum Done {
    Int(i64),
    Str(String),
    Void,
}

/// Ask the poller to report `socket` for `interest`.
fn rearm(poller: &Poller, socket: &Socket, interest: Event) {
    let result = match &socket.kind {
        Kind::Listener(l) => poller.modify(l, interest),
        Kind::Stream { tcp, .. } => poller.modify(tcp, interest),
    };
    result.expect("the socket is registered with the poller");
}

#[cfg(unix)]
fn raw<T: std::os::fd::AsRawFd>(s: &T) -> std::os::fd::RawFd {
    s.as_raw_fd()
}

#[cfg(windows)]
fn raw<T: std::os::windows::io::AsRawSocket>(s: &T) -> std::os::windows::io::RawSocket {
    s.as_raw_socket()
}

/// Record that there is no open socket `id` to `what`, and give the value
/// a failed intrinsic returns.
fn not_connected<T: Default>(what: &str) -> T {
    set_status(
        NOT_CONNECTED,
        format!("could not {what} the socket: it is closed"),
    );
    T::default()
}
