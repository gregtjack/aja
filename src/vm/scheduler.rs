//! The scheduler: the tasks of a run, which take turns on the VM's thread.
//!
//! A task is a green thread, a call stack of its own: its frames and their
//! registers. The running task's are kept in [`Vm::regs`](super::Vm) and
//! [`Vm::frames`](super::Vm), so the interpreter loop reads them directly,
//! and are lent to the scheduler when it switches tasks; the other tasks
//! keep theirs here. A switch swaps the vectors, which moves no values.
//! Natives never call back into Aja, so the Rust stack holds no Aja state
//! and a switch can happen between any two instructions.
//!
//! The interpreter offers a switch at each safepoint: after a call, and
//! after a jump back to the start of a loop. Every loop and every recursion
//! passes one, so a task that never blocks still lets the others run. A
//! task's turn lasts [`SLICE`] safepoints, as Erlang counts reductions.
//! Tasks take turns in the order they became ready, so a run goes the same
//! way every time.
//!
//! A task can also wait: for another task to finish (`Task::join`), or for
//! the tasks of a scope to (the end of `task::scope`). The intrinsic that
//! waits leaves the task's `ip` on its call, so once the task is woken the
//! call runs again and finds what it was waiting for.
//!
//! Or a task can sleep (the built-in `sleep`) until a time. The scheduler wakes
//! the tasks whose time has come whenever it picks the next task to run,
//! and when none can run it puts the VM's thread to sleep until the next
//! one is due.

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, VecDeque},
    rc::Rc,
    time::{Duration, Instant},
};

use crate::ast::Span;

use super::{
    heap::{Gc, Heap},
    native, net,
    value::Value,
};

/// A task's call stack, however a backend keeps one: the VM's is its
/// registers and frames, and a native program's a machine stack. The
/// running task's is lent to the scheduler when it switches tasks, which
/// leaves an empty one (the default) in its place.
pub trait TaskStack: Default {
    /// Whether the task, which is waiting, waits in deferred code that a
    /// block runs on its way out, so that it mustn't be stopped yet.
    fn waits_in_deferred(&self) -> bool;
}

/// How many safepoints a task's turn lasts.
pub const SLICE: u32 = 2000;

/// A task's place in [`Scheduler::tasks`].
pub type TaskId = usize;

/// The functions of `std::task`, and the ones that sleep, which the runtime
/// implements itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Intrinsic {
    OpenScope,
    CloseScope,
    Spawn,
    Join,
    // `Scope::cancel`.
    Cancel,
    // `std::channel`: making a channel, sending, receiving, and closing.
    ChannelOpen,
    Send,
    Receive,
    CloseChannel,
    // Whether a task returned rather than being cancelled, once it has
    // finished: `Task::try_join` is written on it.
    Returned,
    // `sleep(ms)`.
    Sleep,
    // `std::native::sleep_nanos(nanos)`, for `std::time`.
    SleepNanos,
    // The TCP functions, for `std::net`.
    Net(net::Op),
}

impl Intrinsic {
    const ALL: [(Intrinsic, &'static str); 12] = [
        (Intrinsic::OpenScope, "std::task::open_scope"),
        (Intrinsic::CloseScope, "std::task::close_scope"),
        (Intrinsic::Spawn, "std::task::Scope::spawn"),
        (Intrinsic::Join, "std::task::Task::join"),
        (Intrinsic::Cancel, "std::task::Scope::cancel"),
        (Intrinsic::Returned, "std::task::returned"),
        (Intrinsic::ChannelOpen, "std::channel::open"),
        (Intrinsic::Send, "std::channel::put"),
        (Intrinsic::Receive, "std::channel::take"),
        (Intrinsic::CloseChannel, "std::channel::Sender::close"),
        // The built-in, which sleeps only the task that calls it.
        (Intrinsic::Sleep, "sleep"),
        (Intrinsic::SleepNanos, "std::native::sleep_nanos"),
    ];

    /// The intrinsic declared as the native function `name`, if it is one.
    pub fn named(name: &str) -> Option<Intrinsic> {
        let net = net::Op::ALL.iter().map(|&(op, n)| (Intrinsic::Net(op), n));
        Self::ALL
            .iter()
            .copied()
            .chain(net)
            .find(|(_, n)| *n == name)
            .map(|(i, _)| i)
    }

    pub fn name(self) -> &'static str {
        if let Intrinsic::Net(op) = self {
            return net::Op::ALL.iter().find(|(o, _)| *o == op).unwrap().1;
        }
        Self::ALL.iter().find(|(i, _)| *i == self).unwrap().1
    }
}

/// How a call of an [`Intrinsic`] went.
pub enum Outcome {
    /// It returned this value.
    Value(Value),
    /// The task must wait; the call runs again once it is woken.
    Wait,
    /// It stopped the program with this message.
    Error(String),
}

/// Whether there is a task to run after the running one stopped.
pub enum Next {
    /// The next task is running now.
    Switched,
    /// Every task has finished.
    Finished,
    /// Some tasks are waiting, but none can run, so none ever will.
    Deadlock,
}

/// What a waiting task is waiting for, so that only that wakes it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Wait {
    /// The `Task` it is joining to finish.
    Join(Gc),
    /// The tasks of the scope it is closing to finish.
    Close(Gc),
    /// The timer it set, by number, to go off.
    Sleep(u64),
    /// A socket to be ready, or a connect to finish.
    Net,
    /// A receiver to take its value from channel `0`, or room in it.
    Send(Gc),
    /// A value to arrive on channel `0`, or it to close.
    Recv(Gc),
}

impl Wait {
    /// What the task is doing, for a deadlock's report.
    fn describe(self) -> &'static str {
        match self {
            Wait::Join(_) => "joining a task",
            Wait::Close(_) => "waiting for the tasks of its scope to finish",
            Wait::Sleep(_) => "sleeping",
            Wait::Net => "waiting on the network",
            Wait::Send(_) => "sending on a channel",
            Wait::Recv(_) => "receiving from a channel",
        }
    }
}

/// Where a task is in being cancelled.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cancel {
    No,
    /// Its scope was cancelled; it stops at its next safepoint or wait.
    Requested,
    /// It is running its deferred code on the way out of its frames.
    Unwinding,
}

struct Task<S> {
    /// Its call stack, while it isn't running.
    stack: S,
    state: State,
    /// How its last intrinsic that can fail went (see
    /// `std::native::status`), while it isn't running.
    status: (i64, String),
    /// The `Task` that a spawned task's result goes in, and the scope it
    /// was spawned in. `None` for the tasks a run starts with.
    handle: Option<Gc>,
    scope: Option<Gc>,
    /// Set when what the task waited for has happened on its behalf (its
    /// sleep ended, or a receiver took the value it was sending), so its
    /// call, which runs again, returns.
    done: bool,
    /// What it is waiting for, while it waits.
    waiting_on: Option<Wait>,
    cancel: Cancel,
    /// Where it and the tasks it was spawned from were spawned, outermost
    /// first; empty for the tasks a run starts with.
    origin: Rc<[Span]>,
}

enum State {
    /// Running, or in the run queue for its turn.
    Runnable,
    /// Waiting to be woken.
    Waiting,
    /// Finished, with the value its function returned. A spawned task's
    /// value goes in its `Task` instead, and its slot is freed.
    Done(Value),
    /// A slot that a new task can use.
    Free,
}

pub struct Scheduler<S> {
    /// Every task of the current run, by id.
    tasks: Vec<Task<S>>,
    /// The running task.
    current: TaskId,
    /// The tasks waiting for a turn, in the order they get one.
    run_queue: VecDeque<TaskId>,
    /// The slots of finished spawned tasks, to use for new ones.
    free: Vec<TaskId>,
    /// The sleeping tasks, by when each wakes; earliest first, and those
    /// due at the same time in the order they went to sleep.
    timers: BinaryHeap<Reverse<(Instant, u64, TaskId)>>,
    /// How many timers have been set, to order those due at the same time.
    timers_set: u64,
    /// The sockets, and the tasks waiting on them.
    pub net: net::Net,
    /// How many tasks have been asked to stop but haven't started to, so
    /// that checking for one costs nothing while there are none.
    requested: usize,
    /// End every turn at its first safepoint, to flush out code that
    /// depends on when switches happen. Used by tests.
    pub stress: bool,
    /// Each task given a turn, in order.
    #[cfg(test)]
    pub switches: Vec<TaskId>,
}

impl<S: TaskStack> Scheduler<S> {
    pub fn new() -> Self {
        Scheduler {
            tasks: Vec::new(),
            current: 0,
            run_queue: VecDeque::new(),
            free: Vec::new(),
            timers: BinaryHeap::new(),
            timers_set: 0,
            net: net::Net::new(),
            requested: 0,
            stress: false,
            #[cfg(test)]
            switches: Vec::new(),
        }
    }

    /// Forget every task, to start a new run.
    pub fn clear(&mut self) {
        self.tasks.clear();
        self.run_queue.clear();
        self.free.clear();
        self.timers.clear();
        self.net = net::Net::new();
        self.requested = 0;
    }

    /// Wake the running task once `after` has passed. It still has to
    /// [`wait`](Self::wait).
    pub fn set_timer(&mut self, after: Duration) {
        let at = Instant::now() + after;
        self.timers_set += 1;
        self.timers
            .push(Reverse((at, self.timers_set, self.current)));
        self.waiting_for(Wait::Sleep(self.timers_set));
    }

    /// Record what the running task is about to wait for.
    pub fn waiting_for(&mut self, what: Wait) {
        self.tasks[self.current].waiting_on = Some(what);
    }

    /// Wake task `id` if it is waiting for `what`. A cancelled task stops
    /// waiting early, so a timer or join it was waiting for may come later,
    /// when it is waiting for something else or its slot is another task's.
    pub fn wake_for(&mut self, id: TaskId, what: Wait) -> bool {
        let task = &self.tasks[id];
        let waiting = matches!(task.state, State::Waiting) && task.waiting_on == Some(what);
        if waiting {
            self.wake(id);
        }
        waiting
    }

    /// Whether what the running task waited for has just been done for it
    /// (see [`Task::done`]), which then counts as over.
    pub fn take_done(&mut self) -> bool {
        std::mem::take(&mut self.tasks[self.current].done)
    }

    /// Wake task `id`, which is sending on channel `ch`, a receiver having
    /// taken its value.
    pub fn delivered(&mut self, id: TaskId, ch: Gc) {
        debug_assert_eq!(self.tasks[id].waiting_on, Some(Wait::Send(ch)));
        self.tasks[id].done = true;
        self.wake(id);
    }

    /// Wake the first of `receivers` still waiting to receive from `ch`.
    pub fn wake_receiver(&mut self, receivers: &mut std::collections::VecDeque<TaskId>, ch: Gc) {
        while let Some(id) = receivers.pop_front() {
            if self.wake_for(id, Wait::Recv(ch)) {
                break;
            }
        }
    }

    /// Stop task `id`, which has been cancelled, waiting for what it waits
    /// for, dropping its place in any queue: a value it was sending isn't
    /// sent.
    fn forget(&mut self, id: TaskId, heap: &mut Heap) {
        match self.tasks[id].waiting_on {
            Some(Wait::Net) => self.net.forget(id),
            Some(Wait::Send(ch)) => heap.channel(ch).senders.retain(|(t, _)| *t != id),
            Some(Wait::Recv(ch)) => heap.channel(ch).receivers.retain(|t| *t != id),
            _ => {}
        }
    }

    /// Wake the sleeping tasks whose time has come.
    fn wake_sleepers(&mut self) {
        if self.timers.is_empty() {
            return;
        }
        let now = Instant::now();
        while let Some(&Reverse((at, _, id))) = self.timers.peek() {
            if at > now {
                break;
            }
            let Reverse((_, n, _)) = self.timers.pop().unwrap();
            let task = &self.tasks[id];
            if matches!(task.state, State::Waiting) && task.waiting_on == Some(Wait::Sleep(n)) {
                self.tasks[id].done = true;
                self.wake(id);
            }
        }
    }

    /// Wake the tasks whose sockets are ready, waiting up to `timeout`
    /// (forever if `None`) for one to be.
    fn wake_net(&mut self, timeout: Option<Duration>) {
        for id in self.net.poll(timeout) {
            self.wake_for(id, Wait::Net);
        }
    }

    /// Queue the tasks the last network operation woke.
    pub fn wake_net_woken(&mut self) {
        for id in self.net.take_woken() {
            self.wake_for(id, Wait::Net);
        }
    }

    /// The next task to run, waiting for a sleeping one to wake, or a
    /// socket to be ready, if none can run now. `None` if no task can ever
    /// run.
    fn next_task(&mut self) -> Option<TaskId> {
        loop {
            self.wake_sleepers();
            if let Some(next) = self.run_queue.pop_front() {
                return Some(next);
            }
            // A cancelled task's timer wakes no one, so isn't worth
            // waiting for.
            while let Some(&Reverse((_, n, id))) = self.timers.peek() {
                let task = &self.tasks[id];
                if matches!(task.state, State::Waiting) && task.waiting_on == Some(Wait::Sleep(n)) {
                    break;
                }
                self.timers.pop();
            }
            let until = self
                .timers
                .peek()
                .map(|&Reverse((at, _, _))| at.saturating_duration_since(Instant::now()));
            if self.net.is_waiting() {
                self.wake_net(until);
            } else {
                std::thread::sleep(until?);
            }
        }
    }

    /// The running task.
    pub fn current(&self) -> TaskId {
        self.current
    }

    /// A new task whose call stack is `stack`, set up to call a function
    /// that takes no arguments, in the queue to run. A spawned task has a
    /// `handle` for its result and the `scope` it belongs to.
    pub fn spawn(
        &mut self,
        stack: S,
        handle: Option<Gc>,
        scope: Option<Gc>,
        origin: Rc<[Span]>,
    ) -> TaskId {
        let task = Task {
            stack,
            state: State::Runnable,
            status: (0, String::new()),
            handle,
            scope,
            done: false,
            waiting_on: None,
            cancel: Cancel::No,
            origin,
        };
        let id = match self.free.pop() {
            Some(id) => {
                self.tasks[id] = task;
                id
            }
            None => {
                self.tasks.push(task);
                self.tasks.len() - 1
            }
        };
        self.run_queue.push_back(id);
        id
    }

    /// Start running the first task in the queue, putting its stack in
    /// `stack`, which is empty.
    pub fn start(&mut self, stack: &mut S) {
        let first = self.run_queue.pop_front().expect("there is a task to run");
        self.current = first;
        self.resume(first, stack);
    }

    /// How many safepoints a turn lasts.
    pub fn slice(&self) -> u32 {
        if self.stress {
            1
        } else {
            SLICE
        }
    }

    /// End the running task's turn, if another task is waiting for one:
    /// the running task goes to the back of the queue, and the next one's
    /// stack goes in `stack`. In the VM, every frame's `ip` must be up to
    /// date. Returns whether it switched.
    pub fn end_turn(&mut self, stack: &mut S) -> bool {
        if !self.turn_is_wanted() {
            return false;
        }
        let next = self.run_queue.pop_front().expect("a task wants a turn");
        self.run_queue.push_back(self.current);
        self.switch(next, stack);
        true
    }

    /// Whether another task is waiting for a turn, so that
    /// [`end_turn`](Self::end_turn) would switch to it.
    pub fn turn_is_wanted(&mut self) -> bool {
        // Wake the sleepers whose time has come, and the tasks whose
        // sockets are ready, so they get a turn soon even while other tasks
        // compute without waiting.
        self.wake_sleepers();
        if self.net.is_waiting() {
            self.wake_net(Some(Duration::ZERO));
        }
        !self.run_queue.is_empty()
    }

    /// Put waiting task `id` back in the run queue.
    fn wake(&mut self, id: TaskId) {
        debug_assert!(matches!(self.tasks[id].state, State::Waiting));
        let task = &mut self.tasks[id];
        task.state = State::Runnable;
        task.waiting_on = None;
        self.run_queue.push_back(id);
    }

    /// Where the running task and those it was spawned from were spawned.
    pub fn origin(&self) -> Rc<[Span]> {
        self.tasks[self.current].origin.clone()
    }

    /// Whether the running task has been asked to stop and hasn't started
    /// to. Cheap while no task has been.
    pub fn cancel_requested(&self) -> bool {
        self.requested > 0 && self.tasks[self.current].cancel == Cancel::Requested
    }

    /// The running task starts to stop: from now on it runs only its
    /// deferred code, which nothing cancels.
    pub fn start_unwinding(&mut self) {
        let task = &mut self.tasks[self.current];
        debug_assert_eq!(task.cancel, Cancel::Requested);
        task.cancel = Cancel::Unwinding;
        // It may have been woken, then cancelled before its turn: the call
        // it was woken from never ran again, so what that call left to be
        // picked up must not reach the next call the deferred code makes.
        task.done = false;
        self.requested -= 1;
        self.net.forget(self.current);
    }

    /// Cancel scope `s`: each of its tasks, and those in the scopes they
    /// opened, stops at its next safepoint or wait. A task waiting stops
    /// waiting, except one waiting for the tasks of a scope it opened,
    /// which are stopping too and wake it when they have, and one waiting
    /// in deferred code, which runs to its end first.
    pub fn cancel_scope(&mut self, s: Gc, heap: &mut Heap) {
        let mut scopes = vec![s];
        while let Some(s) = scopes.pop() {
            let state = heap.scope(s);
            if state.cancelled {
                continue;
            }
            state.cancelled = true;
            for id in 0..self.tasks.len() {
                let task = &mut self.tasks[id];
                if task.scope != Some(s)
                    || task.cancel != Cancel::No
                    || matches!(task.state, State::Free | State::Done(_))
                {
                    continue;
                }
                task.cancel = Cancel::Requested;
                self.requested += 1;
                // The scopes it opened that still have tasks.
                for other in &self.tasks {
                    if let Some(inner) = other.scope {
                        if heap.scope(inner).owner == id && !scopes.contains(&inner) {
                            scopes.push(inner);
                        }
                    }
                }
                let task = &self.tasks[id];
                if matches!(task.state, State::Waiting)
                    && !matches!(task.waiting_on, Some(Wait::Close(_)))
                    && !task.stack.waits_in_deferred()
                {
                    self.forget(id, heap);
                    self.wake(id);
                }
            }
        }
    }

    /// Make the running task wait until it is woken, and run the next
    /// task. In the VM, every frame's `ip` must be up to date.
    pub fn wait(&mut self, stack: &mut S) -> Next {
        self.tasks[self.current].state = State::Waiting;
        match self.next_task() {
            Some(next) => {
                self.switch(next, stack);
                Next::Switched
            }
            None => Next::Deadlock,
        }
    }

    /// The running task has returned `value` from its outermost frame, so
    /// it is done: a spawned task's value goes in its `Task`, which wakes
    /// the tasks joining it, and the last task of a scope wakes the task
    /// waiting to end it. Then the next task runs.
    pub fn finish(&mut self, value: Option<Value>, heap: &mut Heap, stack: &mut S) -> Next {
        *stack = S::default();
        let task = &mut self.tasks[self.current];
        match task.handle.take() {
            None => {
                task.state =
                    State::Done(value.expect("a task a run starts with can't be cancelled"))
            }
            Some(handle) => {
                task.state = State::Free;
                task.cancel = Cancel::No;
                let scope = task.scope.take().expect("a spawned task has a scope");
                self.free.push(self.current);
                let state = heap.task(handle);
                state.cancelled = value.is_none();
                state.result = value;
                for joiner in std::mem::take(&mut state.joiners) {
                    self.wake_for(joiner, Wait::Join(handle));
                }
                let state = heap.scope(scope);
                state.live -= 1;
                if state.live == 0 {
                    if let Some(closer) = state.closer.take() {
                        self.wake_for(closer, Wait::Close(scope));
                    }
                }
            }
        }
        match self.next_task() {
            Some(next) => {
                self.switch(next, stack);
                Next::Switched
            }
            None if self.tasks.iter().any(|t| matches!(t.state, State::Waiting)) => Next::Deadlock,
            None => Next::Finished,
        }
    }

    /// What each task the run started with returned, once every task has
    /// finished, and forget them.
    pub fn take_results(&mut self) -> Vec<Value> {
        self.run_queue.clear();
        self.free.clear();
        std::mem::take(&mut self.tasks)
            .into_iter()
            .filter_map(|t| match t.state {
                State::Done(v) => Some(v),
                State::Free => None,
                State::Runnable | State::Waiting => unreachable!("every task has finished"),
            })
            .collect()
    }

    /// Each waiting task but the running one: what it waits for, where it
    /// was spawned, and its stack, for a deadlock's report.
    pub fn waiting(&self) -> impl Iterator<Item = (&'static str, &[Span], &S)> {
        self.tasks
            .iter()
            .enumerate()
            .filter(move |(id, t)| *id != self.current && matches!(t.state, State::Waiting))
            .map(|(_, t)| {
                let what = t.waiting_on.map_or("waiting", Wait::describe);
                (what, &t.origin[..], &t.stack)
            })
    }

    /// What the running task is waiting for, for a deadlock's report.
    pub fn current_wait(&self) -> &'static str {
        self.tasks[self.current]
            .waiting_on
            .map_or("waiting", Wait::describe)
    }

    /// The stack of every task but the running one, for the collector.
    pub fn stacks(&self) -> impl Iterator<Item = &S> {
        self.tasks.iter().map(|t| &t.stack)
    }

    /// The results of the finished tasks, which the collector keeps.
    pub fn results(&self) -> impl Iterator<Item = &Value> {
        self.tasks.iter().filter_map(|t| match &t.state {
            State::Done(v) => Some(v),
            _ => None,
        })
    }

    /// The `Task` and `Scope` of each spawned task that hasn't finished,
    /// which the collector keeps.
    pub fn held(&self) -> impl Iterator<Item = Gc> + '_ {
        self.tasks
            .iter()
            .flat_map(|t| t.handle.into_iter().chain(t.scope))
    }

    /// Move the running task's stack (from `stack`) and status into its
    /// [`Task`], and run `next` instead.
    fn switch(&mut self, next: TaskId, stack: &mut S) {
        let task = &mut self.tasks[self.current];
        std::mem::swap(&mut task.stack, stack);
        task.status = native::take_status();
        self.current = next;
        self.resume(next, stack);
    }

    /// Move task `id`'s stack into `stack`, and its status back in place,
    /// to run it.
    fn resume(&mut self, id: TaskId, stack: &mut S) {
        let task = &mut self.tasks[id];
        std::mem::swap(&mut task.stack, stack);
        native::restore_status(std::mem::take(&mut task.status));
        #[cfg(test)]
        self.switches.push(id);
    }
}
