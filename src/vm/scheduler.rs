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

use std::{collections::VecDeque, rc::Rc};

use super::{bytecode::Proto, heap::Gc, native, value::Value, Frame};

/// How many safepoints a task's turn lasts.
pub const SLICE: u32 = 2000;

/// A task's place in [`Scheduler::tasks`].
pub type TaskId = usize;

struct Task {
    /// Its registers and frames, while it isn't running.
    regs: Vec<Value>,
    frames: Vec<Frame>,
    state: State,
    /// How its last intrinsic that can fail went (see
    /// `std::native::status`), while it isn't running.
    status: (i64, String),
}

enum State {
    /// Running, or waiting in the run queue for its turn.
    Runnable,
    /// Finished, with the value its function returned.
    Done(Value),
}

pub struct Scheduler {
    /// Every task of the current run, by id.
    tasks: Vec<Task>,
    /// The running task.
    current: TaskId,
    /// The tasks waiting for a turn, in the order they get one.
    run_queue: VecDeque<TaskId>,
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
            stress: false,
            #[cfg(test)]
            switches: Vec::new(),
        }
    }

    /// Forget every task, to start a new run.
    pub fn clear(&mut self) {
        self.tasks.clear();
        self.run_queue.clear();
    }

    /// A new task that calls `closure`, whose code is `proto` and which
    /// takes no arguments, in the queue to run.
    pub fn spawn(&mut self, closure: Gc, proto: Rc<Proto>) -> TaskId {
        let regs = vec![Value::Void; proto.num_regs as usize];
        let frames = vec![Frame {
            closure,
            proto,
            ip: 0,
            base: 0,
            ret: 0,
            tail_call: None,
        }];
        self.tasks.push(Task {
            regs,
            frames,
            state: State::Runnable,
            status: (0, String::new()),
        });
        let id = self.tasks.len() - 1;
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
        let Some(next) = self.run_queue.pop_front() else {
            return false;
        };
        self.run_queue.push_back(self.current);
        self.switch(next, regs, frames);
        true
    }

    /// The running task has returned `value` from its outermost frame, so
    /// it is done; run the next task. Returns false if there is none.
    pub fn finish(&mut self, value: Value, regs: &mut Vec<Value>, frames: &mut Vec<Frame>) -> bool {
        self.tasks[self.current].state = State::Done(value);
        regs.clear();
        let Some(next) = self.run_queue.pop_front() else {
            return false;
        };
        self.switch(next, regs, frames);
        true
    }

    /// What each task returned, once every one has finished, and forget
    /// them.
    pub fn take_results(&mut self) -> Vec<Value> {
        self.run_queue.clear();
        std::mem::take(&mut self.tasks)
            .into_iter()
            .map(|t| match t.state {
                State::Done(v) => v,
                State::Runnable => unreachable!("every task has finished"),
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
            State::Runnable => None,
        })
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
