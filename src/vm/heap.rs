//! The VM's garbage-collected heap.
//!
//! Every object that can refer to other objects (today: closures and the
//! cells that hold captured variables) lives here and is referred to by a
//! [`Gc`] handle. The collector is a precise, non-moving mark-and-sweep: the VM
//! knows exactly where every value is (registers, globals, call frames), so
//! it marks everything reachable from those roots and frees the rest,
//! cycles included.
//!
//! Strings hold no references, so they cannot be part of a cycle; they stay
//! reference-counted (`Rc<str>`) and are freed as soon as they are unused.
//!
//! The collector runs only when the VM allocates, at points where every live
//! value is in a register, global, or frame. It runs once the number of live
//! objects has doubled since the last collection.

use std::rc::Rc;

use super::{bytecode::Proto, value::Value};

/// A handle to an object on the heap.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Gc(u32);

impl std::fmt::Display for Gc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

/// A function value: compiled code plus the cells it captured.
pub struct Closure {
    pub proto: Rc<Proto>,
    pub upvalues: Box<[Gc]>,
}

enum Obj {
    Closure(Closure),
    /// A captured variable.
    Cell(Value),
}

/// Collect once this many objects are live, at the least.
const MIN_THRESHOLD: usize = 1024;

pub struct Heap {
    /// Objects by handle; `None` for a free slot.
    objects: Vec<Option<Obj>>,
    marks: Vec<bool>,
    /// Free slots, reused before the heap grows.
    free: Vec<u32>,
    live: usize,
    /// Collect when `live` reaches this.
    threshold: usize,
    /// Collect before every allocation, to flush out values the VM forgot to
    /// keep reachable. Used by tests.
    pub stress: bool,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            objects: Vec::new(),
            marks: Vec::new(),
            free: Vec::new(),
            live: 0,
            threshold: MIN_THRESHOLD,
            stress: false,
        }
    }

    /// The number of objects not yet freed.
    #[cfg(test)]
    pub fn live(&self) -> usize {
        self.live
    }

    /// Whether the VM should collect before its next allocation.
    pub fn wants_collection(&self) -> bool {
        self.stress || self.live >= self.threshold
    }

    pub fn alloc_closure(&mut self, closure: Closure) -> Gc {
        self.alloc(Obj::Closure(closure))
    }

    pub fn alloc_cell(&mut self, value: Value) -> Gc {
        self.alloc(Obj::Cell(value))
    }

    fn alloc(&mut self, obj: Obj) -> Gc {
        self.live += 1;
        match self.free.pop() {
            Some(i) => {
                self.objects[i as usize] = Some(obj);
                Gc(i)
            }
            None => {
                let i = u32::try_from(self.objects.len()).expect("the heap is full");
                self.objects.push(Some(obj));
                self.marks.push(false);
                Gc(i)
            }
        }
    }

    fn get(&self, gc: Gc) -> &Obj {
        match &self.objects[gc.0 as usize] {
            Some(obj) => obj,
            None => panic!("internal error: object {gc} was used after being freed"),
        }
    }

    pub fn closure(&self, gc: Gc) -> &Closure {
        match self.get(gc) {
            Obj::Closure(c) => c,
            Obj::Cell(_) => panic!("internal error: object {gc} is a cell, not a closure"),
        }
    }

    pub fn cell(&self, gc: Gc) -> &Value {
        match self.get(gc) {
            Obj::Cell(v) => v,
            Obj::Closure(_) => panic!("internal error: object {gc} is a closure, not a cell"),
        }
    }

    pub fn set_cell(&mut self, gc: Gc, value: Value) {
        match &mut self.objects[gc.0 as usize] {
            Some(Obj::Cell(v)) => *v = value,
            _ => panic!("internal error: object {gc} is not a cell"),
        }
    }

    /// Free every object not reachable from `roots` or `extra`.
    pub fn collect<'a>(&mut self, roots: impl IntoIterator<Item = &'a Value>, extra: impl IntoIterator<Item = Gc>) {
        // Mark, with an explicit worklist so long chains can't overflow the stack.
        let mut work: Vec<Gc> = roots.into_iter().filter_map(Value::handle).collect();
        work.extend(extra);
        while let Some(gc) = work.pop() {
            let i = gc.0 as usize;
            if self.marks[i] {
                continue;
            }
            self.marks[i] = true;
            match self.get(gc) {
                Obj::Closure(c) => work.extend(c.upvalues.iter().copied()),
                Obj::Cell(v) => work.extend(v.handle()),
            }
        }

        // Sweep.
        for (i, (obj, marked)) in self.objects.iter_mut().zip(&mut self.marks).enumerate() {
            if obj.is_some() && !*marked {
                *obj = None;
                self.free.push(i as u32);
                self.live -= 1;
            }
            *marked = false;
        }
        self.threshold = (self.live * 2).max(MIN_THRESHOLD);
    }

    /// How `print` shows a value.
    pub fn display(&self, value: &Value) -> String {
        match value {
            Value::Void => "()".to_string(),
            Value::Int(i) => i.to_string(),
            Value::Float(n) => format!("{n:?}"),
            Value::Bool(b) => b.to_string(),
            Value::Str(s) => s.to_string(),
            Value::Fn(gc) => format!("<fn {}>", self.closure(*gc).proto.signature),
            Value::Native(builtin, _) => format!("<native fn: {}>", builtin.name),
            Value::Cell(gc) => self.display(self.cell(*gc)),
        }
    }

    /// How the REPL shows a value: like `print`, but with strings quoted.
    pub fn repr(&self, value: &Value) -> String {
        match value {
            Value::Str(s) => format!("{:?}", &**s),
            other => self.display(other),
        }
    }
}
