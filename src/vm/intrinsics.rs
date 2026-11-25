//! The intrinsics: the functions of `std::task`, `std::channel`, and
//! `std::net`, and the ones that sleep, which start, wait for, or wake
//! tasks, and so need the scheduler. Both backends run them through
//! [`call`], each as a [`Host`].

use std::rc::Rc;

use crate::ast::Span;

use super::{
    heap::{Gc, Heap},
    internal_error, native,
    scheduler::{Intrinsic, Outcome, Scheduler, TaskStack, Wait},
    value::Value,
};

/// What runs a program: the VM, or a native program's runtime.
pub(crate) trait Host {
    type Stack: TaskStack;

    fn parts(&mut self) -> (&mut Heap, &mut Scheduler<Self::Stack>);

    /// Collect garbage if the heap has grown enough to want it. The
    /// intrinsic's arguments are still in their registers, so they stay.
    fn collect_if_wanted(&mut self);

    /// Where in the source the intrinsic is being called.
    fn call_site(&self) -> Span;

    /// The stack of a new task that will call `closure`, which takes no
    /// arguments.
    fn new_stack(&mut self, closure: Gc) -> Self::Stack;
}

/// Call the intrinsic `f` with `args` for the running task.
pub(crate) fn call<H: Host>(host: &mut H, f: Intrinsic, args: &[Value]) -> Outcome {
    let arg = |i: usize| args[i].clone();
    let (heap, scheduler) = host.parts();
    match f {
        Intrinsic::OpenScope => {
            host.collect_if_wanted();
            let (heap, scheduler) = host.parts();
            let owner = scheduler.current();
            Outcome::Value(Value::Scope(heap.alloc_scope(owner)))
        }
        Intrinsic::CloseScope => {
            let Value::Scope(s) = arg(0) else {
                internal_error("Scope", &arg(0));
            };
            let current = scheduler.current();
            let scope = heap.scope(s);
            if scope.live == 0 {
                scope.ended = true;
                Outcome::Value(Value::Void)
            } else {
                scope.closer = Some(current);
                scheduler.waiting_for(Wait::Close(s));
                Outcome::Wait
            }
        }
        Intrinsic::Spawn => {
            let (Value::Scope(s), Value::Fn(f)) = (arg(0), arg(1)) else {
                internal_error("Scope and Fn", &arg(1));
            };
            if heap.scope(s).ended {
                return Outcome::Error(
                    "cannot spawn a task in a scope that has ended: a Scope can only be \
                     used inside the `task::scope` call that made it"
                        .to_string(),
                );
            }
            // What the checker can't see: a function from a parameter,
            // a list, or a call.
            if heap.reaches_cell(&Value::Fn(f)) {
                return Outcome::Error(
                    "cannot spawn this function: it can reach a `var` of the task \
                     spawning it, and a `var` belongs to one task (give the task copies \
                     of what it needs, made with `let`)"
                        .to_string(),
                );
            }
            // The scope and the function are still in their registers,
            // so a collection here keeps them.
            host.collect_if_wanted();
            let (heap, _) = host.parts();
            let handle = heap.alloc_task();
            // In a cancelled scope, a task never starts.
            if heap.scope(s).cancelled {
                heap.task(handle).cancelled = true;
                return Outcome::Value(Value::Task(handle));
            }
            heap.scope(s).live += 1;
            let at = host.call_site();
            let stack = host.new_stack(f);
            let (_, scheduler) = host.parts();
            let mut origin = scheduler.origin().to_vec();
            origin.push(at);
            scheduler.spawn(stack, Some(handle), Some(s), Rc::from(origin));
            Outcome::Value(Value::Task(handle))
        }
        Intrinsic::ChannelOpen => {
            let Value::Int(capacity) = arg(0) else {
                internal_error("Int", &arg(0));
            };
            if capacity < 0 {
                return Outcome::Error(format!(
                    "cannot make a channel with capacity {capacity}: it must be 0 or more"
                ));
            }
            host.collect_if_wanted();
            let (heap, _) = host.parts();
            Outcome::Value(Value::Channel(heap.alloc_channel(capacity as usize)))
        }
        Intrinsic::Send => {
            let (Value::Channel(ch), value) = (arg(0), arg(1)) else {
                internal_error("Channel", &arg(0));
            };
            // Run again after a receiver took the value.
            if scheduler.take_done() {
                native::set_status(native::OK, String::new());
                return Outcome::Value(Value::Void);
            }
            if heap.reaches_cell(&value) {
                return Outcome::Error(
                    "cannot send this value: it can reach a `var` of the task sending it, \
                     and a `var` belongs to one task (send a copy, made with `let`)"
                        .to_string(),
                );
            }
            let current = scheduler.current();
            let c = heap.channel(ch);
            if c.closed {
                native::set_status(native::END_OF_INPUT, "the channel is closed".to_string());
                return Outcome::Value(Value::Void);
            }
            native::set_status(native::OK, String::new());
            if c.buffer.len() < c.capacity {
                c.buffer.push_back(value);
                scheduler.wake_receiver(&mut c.receivers, ch);
                Outcome::Value(Value::Void)
            } else {
                // Waits until a receiver takes it: at once, for a
                // receiver already waiting on a channel that holds none.
                c.senders.push_back((current, value));
                scheduler.wake_receiver(&mut c.receivers, ch);
                scheduler.waiting_for(Wait::Send(ch));
                Outcome::Wait
            }
        }
        Intrinsic::Receive => {
            let Value::Channel(ch) = arg(0) else {
                internal_error("Channel", &arg(0));
            };
            let current = scheduler.current();
            let c = heap.channel(ch);
            let got = match c.buffer.pop_front() {
                // A waiting sender's value takes the room this leaves.
                Some(v) => {
                    if let Some((sender, next)) = c.senders.pop_front() {
                        c.buffer.push_back(next);
                        scheduler.delivered(sender, ch);
                    }
                    Some(v)
                }
                None => match c.senders.pop_front() {
                    Some((sender, v)) => {
                        scheduler.delivered(sender, ch);
                        Some(v)
                    }
                    None => None,
                },
            };
            match got {
                Some(v) => {
                    native::set_status(native::OK, String::new());
                    Outcome::Value(v)
                }
                None if c.closed => {
                    native::set_status(native::END_OF_INPUT, String::new());
                    Outcome::Value(Value::Void)
                }
                None => {
                    c.receivers.push_back(current);
                    scheduler.waiting_for(Wait::Recv(ch));
                    Outcome::Wait
                }
            }
        }
        Intrinsic::CloseChannel => {
            let Value::Channel(ch) = arg(0) else {
                internal_error("Channel", &arg(0));
            };
            let c = heap.channel(ch);
            c.closed = true;
            // Receivers get what is left, then `None`; senders still
            // waiting find it closed and keep their values.
            for id in std::mem::take(&mut c.receivers) {
                scheduler.wake_for(id, Wait::Recv(ch));
            }
            for (id, _) in std::mem::take(&mut c.senders) {
                scheduler.wake_for(id, Wait::Send(ch));
            }
            Outcome::Value(Value::Void)
        }
        Intrinsic::Cancel => {
            let Value::Scope(s) = arg(0) else {
                internal_error("Scope", &arg(0));
            };
            if !heap.scope(s).ended {
                scheduler.cancel_scope(s, heap);
            }
            Outcome::Value(Value::Void)
        }
        Intrinsic::Sleep | Intrinsic::SleepNanos => {
            let Value::Int(n) = arg(0) else {
                internal_error("Int", &arg(0));
            };
            let (unit, duration): (_, fn(u64) -> std::time::Duration) = if f == Intrinsic::Sleep {
                ("milliseconds", std::time::Duration::from_millis)
            } else {
                ("nanoseconds", std::time::Duration::from_nanos)
            };
            if n < 0 {
                return Outcome::Error(format!(
                    "cannot sleep for {n} {unit}: the time must be 0 or more"
                ));
            }
            // The call runs again when the task wakes, and then returns.
            if scheduler.take_done() || n == 0 {
                return Outcome::Value(Value::Void);
            }
            scheduler.set_timer(duration(n as u64));
            Outcome::Wait
        }
        Intrinsic::Net(op) => {
            let args = args[..op.arity()].to_vec();
            let task = scheduler.current();
            let outcome = scheduler.net.call(op, task, heap, &args);
            scheduler.wake_net_woken();
            if let Outcome::Wait = outcome {
                scheduler.waiting_for(Wait::Net);
            }
            outcome
        }
        Intrinsic::Join => {
            let Value::Task(t) = arg(0) else {
                internal_error("Task", &arg(0));
            };
            let current = scheduler.current();
            let task = heap.task(t);
            match &task.result {
                Some(v) => Outcome::Value(v.clone()),
                None if task.cancelled => Outcome::Error(
                    "cannot join a task that was cancelled: it stopped before returning a \
                     value (`try_join` gives `None` for one)"
                        .to_string(),
                ),
                None => {
                    task.joiners.push(current);
                    scheduler.waiting_for(Wait::Join(t));
                    Outcome::Wait
                }
            }
        }
        Intrinsic::Returned => {
            let Value::Task(t) = arg(0) else {
                internal_error("Task", &arg(0));
            };
            let current = scheduler.current();
            let task = heap.task(t);
            if task.result.is_some() || task.cancelled {
                Outcome::Value(Value::Bool(!task.cancelled))
            } else {
                task.joiners.push(current);
                scheduler.waiting_for(Wait::Join(t));
                Outcome::Wait
            }
        }
    }
}
