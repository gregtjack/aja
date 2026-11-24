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

use super::{
    bytecode::Proto,
    heap::{Gc, Heap},
    native, net,
    value::Value,
    Frame,
};

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
    // `sleep(ms)`.
    Sleep,
    // `std::native::sleep_nanos(nanos)`, for `std::time`.
    SleepNanos,
    // The TCP functions, for `std::net`.
    Net(net::Op),
}

impl Intrinsic {
    const ALL: [(Intrinsic, &'static str); 6] = [
        (Intrinsic::OpenScope, "std::task::open_scope"),
        (Intrinsic::CloseScope, "std::task::close_scope"),
        (Intrinsic::Spawn, "std::task::Scope::spawn"),
        (Intrinsic::Join, "std::task::Task::join"),
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

struct Task {
    /// Its registers and frames, while it isn't running.
    regs: Vec<Value>,
    frames: Vec<Frame>,
    state: State,
    /// How its last intrinsic that can fail went (see
    /// `std::native::status`), while it isn't running.
    status: (i64, String),
    /// The `Task` that a spawned task's result goes in, and the scope it
    /// was spawned in. `None` for the tasks a run starts with.
    handle: Option<Gc>,
    scope: Option<Gc>,
    /// Set when the task's sleep has ended, so its call of `sleep`, which
    /// runs again, returns.
    slept: bool,
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

pub struct Scheduler {
    /// Every task of the current run, by id.
    tasks: Vec<Task>,
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
    /// End every turn at its first safepoint, to flush out code that
    /// depends on when switches happen. Used by tests.
    pub stress: bool,
    /// Each task given a turn, in order.
    #[cfg(test)]
    pub switches: Vec<TaskId>,
}

impl Scheduler {
    pub fn new() -> Self {
        Scheduler {
            tasks: Vec::new(),
            current: 0,
            run_queue: VecDeque::new(),
            free: Vec::new(),
            timers: BinaryHeap::new(),
            timers_set: 0,
            net: net::Net::new(),
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
    }

    /// Wake the running task once `after` has passed. It still has to
    /// [`wait`](Self::wait).
    pub fn set_timer(&mut self, after: Duration) {
        let at = Instant::now() + after;
        self.timers_set += 1;
        self.timers
            .push(Reverse((at, self.timers_set, self.current)));
    }

    /// Whether the running task has just woken from a sleep, which then
    /// counts as over.
    pub fn take_slept(&mut self) -> bool {
        std::mem::take(&mut self.tasks[self.current].slept)
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
            self.timers.pop();
            self.tasks[id].slept = true;
            self.wake(id);
        }
    }

    /// Wake the tasks whose sockets are ready, waiting up to `timeout`
    /// (forever if `None`) for one to be.
    fn wake_net(&mut self, timeout: Option<Duration>) {
        for id in self.net.poll(timeout) {
            self.wake(id);
        }
    }

    /// Queue the tasks the last network operation woke.
    pub fn wake_net_woken(&mut self) {
        for id in self.net.take_woken() {
            self.wake(id);
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

    /// A new task that calls `closure`, whose code is `proto` and which
    /// takes no arguments, in the queue to run. A spawned task has a
    /// `handle` for its result and the `scope` it belongs to.
    pub fn spawn(
        &mut self,
        closure: Gc,
        proto: Rc<Proto>,
        handle: Option<Gc>,
        scope: Option<Gc>,
    ) -> TaskId {
        let regs = vec![Value::Void; proto.num_regs as usize];
        let frames = vec![Frame {
            closure,
            proto,
            ip: 0,
            base: 0,
            ret: 0,
            tail_call: None,
        }];
        let task = Task {
            regs,
            frames,
            state: State::Runnable,
            status: (0, String::new()),
            handle,
            scope,
            slept: false,
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
    /// `regs` and `frames`, which are empty.
    pub fn start(&mut self, regs: &mut Vec<Value>, frames: &mut Vec<Frame>) {
        let first = self.run_queue.pop_front().expect("there is a task to run");
        self.current = first;
        self.resume(first, regs, frames);
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
    /// stack goes in `regs` and `frames`. Every frame's `ip` must be up to
    /// date. Returns whether it switched.
    pub fn end_turn(&mut self, regs: &mut Vec<Value>, frames: &mut Vec<Frame>) -> bool {
        // Wake the sleepers whose time has come, and the tasks whose
        // sockets are ready, so they get a turn soon even while other tasks
        // compute without waiting.
        self.wake_sleepers();
        if self.net.is_waiting() {
            self.wake_net(Some(Duration::ZERO));
        }
        let Some(next) = self.run_queue.pop_front() else {
            return false;
        };
        self.run_queue.push_back(self.current);
        self.switch(next, regs, frames);
        true
    }

    /// Put waiting task `id` back in the run queue.
    pub fn wake(&mut self, id: TaskId) {
        debug_assert!(matches!(self.tasks[id].state, State::Waiting));
        self.tasks[id].state = State::Runnable;
        self.run_queue.push_back(id);
    }

    /// Make the running task wait until it is woken, and run the next
    /// task. Every frame's `ip` must be up to date.
    pub fn wait(&mut self, regs: &mut Vec<Value>, frames: &mut Vec<Frame>) -> Next {
        self.tasks[self.current].state = State::Waiting;
        match self.next_task() {
            Some(next) => {
                self.switch(next, regs, frames);
                Next::Switched
            }
            None => Next::Deadlock,
        }
    }

    /// The running task has returned `value` from its outermost frame, so
    /// it is done: a spawned task's value goes in its `Task`, which wakes
    /// the tasks joining it, and the last task of a scope wakes the task
    /// waiting to end it. Then the next task runs.
    pub fn finish(
        &mut self,
        value: Value,
        heap: &mut Heap,
        regs: &mut Vec<Value>,
        frames: &mut Vec<Frame>,
    ) -> Next {
        regs.clear();
        frames.clear();
        let task = &mut self.tasks[self.current];
        match task.handle.take() {
            None => task.state = State::Done(value),
            Some(handle) => {
                task.state = State::Free;
                let scope = task.scope.take().expect("a spawned task has a scope");
                self.free.push(self.current);
                let state = heap.task(handle);
                state.result = Some(value);
                for joiner in std::mem::take(&mut state.joiners) {
                    self.wake(joiner);
                }
                let scope = heap.scope(scope);
                scope.live -= 1;
                if scope.live == 0 {
                    if let Some(closer) = scope.closer.take() {
                        self.wake(closer);
                    }
                }
            }
        }
        match self.next_task() {
            Some(next) => {
                self.switch(next, regs, frames);
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

    /// The registers and frames of every task but the running one, for the
    /// collector.
    pub fn stacks(&self) -> impl Iterator<Item = (&[Value], &[Frame])> {
        self.tasks.iter().map(|t| (&t.regs[..], &t.frames[..]))
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

    /// Move the running task's stack (from `regs` and `frames`) and status
    /// into its [`Task`], and run `next` instead.
    fn switch(&mut self, next: TaskId, regs: &mut Vec<Value>, frames: &mut Vec<Frame>) {
        let task = &mut self.tasks[self.current];
        std::mem::swap(&mut task.regs, regs);
        std::mem::swap(&mut task.frames, frames);
        task.status = native::take_status();
        self.current = next;
        self.resume(next, regs, frames);
    }

    /// Move task `id`'s stack into `regs` and `frames`, and its status back
    /// in place, to run it.
    fn resume(&mut self, id: TaskId, regs: &mut Vec<Value>, frames: &mut Vec<Frame>) {
        let task = &mut self.tasks[id];
        std::mem::swap(&mut task.regs, regs);
        std::mem::swap(&mut task.frames, frames);
        native::restore_status(std::mem::take(&mut task.status));
        #[cfg(test)]
        self.switches.push(id);
    }
}
