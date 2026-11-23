//! The VM's garbage-collected heap.
//!
//! Every value that isn't a plain scalar (closures, the cells that hold
//! captured variables, struct and enum values, and strings) lives here and is
//! referred to by a [`Gc`] handle. The collector is a precise, non-moving mark-and-sweep: the VM
//! knows exactly where every value is (registers, globals, call frames), so
//! it marks everything reachable from those roots and frees the rest,
//! cycles included.
//!
//! Strings are heap objects like any other, though they refer to nothing.
//! String literals are allocated once, when the code that uses them is
//! loaded, and stay alive as roots; strings built at runtime (by `+`) are
//! freed once unreachable.
//!
//! The collector runs only when the VM allocates, at points where every live
//! value is in a register, global, or frame. It runs once the heap's size in
//! bytes has doubled since the last collection, so a few large strings
//! trigger it as surely as many small objects.

use std::rc::Rc;

use super::{
    bytecode::{Layout, Proto},
    value::Value,
};

/// A handle to an object on the heap.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Gc(u32);

impl std::fmt::Display for Gc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

/// A function value: compiled code plus the values it captured (a cell, for
/// a captured `var`).
pub struct Closure {
    pub proto: Rc<Proto>,
    pub upvalues: Box<[Value]>,
}

enum Obj {
    Closure(Closure),
    /// A captured variable.
    Cell(Value),
    /// A struct or enum value. Its fields never change after it is made.
    Data(Rc<Layout>, Box<[Value]>),
    /// A string. Its contents never change after it is made.
    Str(Box<str>),
}

impl Obj {
    /// Roughly how many bytes the object uses, for deciding when to collect.
    fn size(&self) -> usize {
        size_of::<Option<Obj>>()
            + match self {
                Obj::Closure(c) => c.upvalues.len() * size_of::<Value>(),
                Obj::Cell(_) => 0,
                Obj::Data(_, fields) => fields.len() * size_of::<Value>(),
                Obj::Str(s) => s.len(),
            }
    }
}

/// Collect once the heap uses this many bytes, at the least: about a
/// thousand small objects.
const MIN_THRESHOLD: usize = 64 << 10;

/// The longest string, in bytes. Lengths fit in an `int`.
pub const MAX_STR_LEN: usize = i32::MAX as usize;

pub struct Heap {
    /// Objects by handle; `None` for a free slot.
    objects: Vec<Option<Obj>>,
    marks: Vec<bool>,
    /// Free slots, reused before the heap grows.
    free: Vec<u32>,
    live: usize,
    /// The total [`Obj::size`] of the objects not yet freed.
    bytes: usize,
    /// Collect when `bytes` would pass this.
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
            bytes: 0,
            threshold: MIN_THRESHOLD,
            stress: false,
        }
    }

    /// The number of objects not yet freed.
    #[cfg(test)]
    pub fn live(&self) -> usize {
        self.live
    }

    /// The bytes used by objects not yet freed.
    #[cfg(test)]
    pub fn bytes(&self) -> usize {
        self.bytes
    }

    /// Whether the VM should collect before allocating an object that holds
    /// `extra` bytes besides its fixed size (a string's length, say).
    pub fn wants_collection(&self, extra: usize) -> bool {
        self.stress || self.bytes + size_of::<Option<Obj>>() + extra > self.threshold
    }

    pub fn alloc_closure(&mut self, closure: Closure) -> Gc {
        self.alloc(Obj::Closure(closure))
    }

    pub fn alloc_cell(&mut self, value: Value) -> Gc {
        self.alloc(Obj::Cell(value))
    }

    pub fn alloc_data(&mut self, layout: Rc<Layout>, fields: Box<[Value]>) -> Gc {
        self.alloc(Obj::Data(layout, fields))
    }

    /// A new string. Longer than [`MAX_STR_LEN`] is the caller's error to report.
    pub fn alloc_str(&mut self, s: Box<str>) -> Gc {
        debug_assert!(s.len() <= MAX_STR_LEN);
        self.alloc(Obj::Str(s))
    }

    fn alloc(&mut self, obj: Obj) -> Gc {
        self.live += 1;
        self.bytes += obj.size();
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
            _ => panic!("internal error: object {gc} is not a closure"),
        }
    }

    pub fn cell(&self, gc: Gc) -> &Value {
        match self.get(gc) {
            Obj::Cell(v) => v,
            _ => panic!("internal error: object {gc} is not a cell"),
        }
    }

    /// A struct or enum value's layout and fields.
    pub fn data(&self, gc: Gc) -> (&Layout, &[Value]) {
        match self.get(gc) {
            Obj::Data(layout, fields) => (layout, fields),
            _ => panic!("internal error: object {gc} is not a struct or enum value"),
        }
    }

    pub fn str(&self, gc: Gc) -> &str {
        match self.get(gc) {
            Obj::Str(s) => s,
            _ => panic!("internal error: object {gc} is not a string"),
        }
    }

    /// Whether two values of the same type are equal, comparing structs and
    /// enums field by field. Iterative, so long lists can't overflow the stack.
    pub fn equal(&self, a: &Value, b: &Value) -> bool {
        let mut work = vec![(a.clone(), b.clone())];
        while let Some((a, b)) = work.pop() {
            let same = match (&a, &b) {
                (Value::Data(x), Value::Data(y)) if x == y => true,
                (Value::Data(x), Value::Data(y)) => {
                    let ((lx, fx), (ly, fy)) = (self.data(*x), self.data(*y));
                    if lx.tag != ly.tag {
                        return false;
                    }
                    work.extend(fx.iter().cloned().zip(fy.iter().cloned()));
                    true
                }
                (Value::Int(x), Value::Int(y)) => x == y,
                (Value::Float(x), Value::Float(y)) => x == y,
                (Value::Bool(x), Value::Bool(y)) => x == y,
                (Value::Str(x), Value::Str(y)) => x == y || self.str(*x) == self.str(*y),
                (Value::Void, Value::Void) => true,
                // The type checker only allows `==` on types made of the above.
                _ => panic!("internal error: comparing {a:?} with {b:?}"),
            };
            if !same {
                return false;
            }
        }
        true
    }

    pub fn set_cell(&mut self, gc: Gc, value: Value) {
        match &mut self.objects[gc.0 as usize] {
            Some(Obj::Cell(v)) => *v = value,
            _ => panic!("internal error: object {gc} is not a cell"),
        }
    }

    /// Fill in a captured value of a closure that was just made, for a
    /// closure that captures itself ([`Capture::Itself`]).
    ///
    /// [`Capture::Itself`]: super::bytecode::Capture::Itself
    pub fn set_upvalue(&mut self, gc: Gc, index: usize, value: Value) {
        match &mut self.objects[gc.0 as usize] {
            Some(Obj::Closure(c)) => c.upvalues[index] = value,
            _ => panic!("internal error: object {gc} is not a closure"),
        }
    }

    /// Free every object not reachable from `roots` or `extra`.
    pub fn collect<'a>(
        &mut self,
        roots: impl IntoIterator<Item = &'a Value>,
        extra: impl IntoIterator<Item = Gc>,
    ) {
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
                Obj::Closure(c) => work.extend(c.upvalues.iter().filter_map(Value::handle)),
                Obj::Cell(v) => work.extend(v.handle()),
                Obj::Data(_, fields) => work.extend(fields.iter().filter_map(Value::handle)),
                Obj::Str(_) => {}
            }
        }

        // Sweep.
        for (i, (obj, marked)) in self.objects.iter_mut().zip(&mut self.marks).enumerate() {
            if !*marked {
                if let Some(dead) = obj.take() {
                    self.free.push(i as u32);
                    self.live -= 1;
                    self.bytes -= dead.size();
                }
            }
            *marked = false;
        }
        self.threshold = (self.bytes * 2).max(MIN_THRESHOLD);
    }

    /// How `print` shows a value. Strings inside structs and enums are quoted.
    pub fn display(&self, value: &Value) -> String {
        self.show(value, false)
    }

    /// How the REPL shows a value: like `print`, but with strings quoted.
    pub fn repr(&self, value: &Value) -> String {
        self.show(value, true)
    }

    /// Show a value, e.g. `Shape::Rect(Point { x: 1, y: 2 }, 3.5)`. This
    /// works from an explicit stack, so deeply nested values (long lists)
    /// can't overflow the Rust stack.
    fn show(&self, value: &Value, quote: bool) -> String {
        enum Item<'a> {
            Value(&'a Value, bool),
            Text(&'a str),
        }
        let mut out = String::new();
        let mut stack = vec![Item::Value(value, quote)];
        while let Some(item) = stack.pop() {
            let value = match item {
                Item::Text(t) => {
                    out.push_str(t);
                    continue;
                }
                Item::Value(v, quote) => match v {
                    Value::Str(gc) if quote => format!("{:?}", self.str(*gc)),
                    Value::Cell(gc) => {
                        stack.push(Item::Value(self.cell(*gc), quote));
                        continue;
                    }
                    Value::Data(gc) => {
                        let (layout, fields) = self.data(*gc);
                        out.push_str(&layout.name);
                        if fields.is_empty() {
                            continue;
                        }
                        let mut items = Vec::new();
                        match &layout.fields {
                            Some(names) => {
                                items.push(Item::Text(" { "));
                                for (i, (name, v)) in names.iter().zip(fields.iter()).enumerate() {
                                    if i > 0 {
                                        items.push(Item::Text(", "));
                                    }
                                    items.push(Item::Text(name));
                                    items.push(Item::Text(": "));
                                    items.push(Item::Value(v, true));
                                }
                                items.push(Item::Text(" }"));
                            }
                            None => {
                                items.push(Item::Text("("));
                                for (i, v) in fields.iter().enumerate() {
                                    if i > 0 {
                                        items.push(Item::Text(", "));
                                    }
                                    items.push(Item::Value(v, true));
                                }
                                items.push(Item::Text(")"));
                            }
                        }
                        stack.extend(items.into_iter().rev());
                        continue;
                    }
                    Value::Void => "()".to_string(),
                    Value::Int(i) => i.to_string(),
                    Value::Float(n) => format!("{n:?}"),
                    Value::Bool(b) => b.to_string(),
                    Value::Str(gc) => {
                        out.push_str(self.str(*gc));
                        continue;
                    }
                    Value::Fn(gc) => format!("<fn {}>", self.closure(*gc).proto.signature),
                    Value::Native(builtin, _) => format!("<native fn: {}>", builtin.name),
                },
            };
            out.push_str(&value);
        }
        out
    }
}
