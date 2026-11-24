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

use std::{
    cmp::Ordering,
    collections::HashSet,
    hash::{DefaultHasher, Hash, Hasher},
    rc::Rc,
};

use super::{
    bytecode::{Layout, Proto},
    hamt::Hamt,
    list::List,
    scheduler::TaskId,
    sorted::Tree,
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
    /// A list. It never changes after it is made, but shares parts with
    /// the lists made from it.
    List(List),
    /// A `std::collections::Map`, which never changes after it is made.
    Map(Hamt),
    /// A `std::collections::Set`: a map whose values are all `void`.
    Set(Hamt),
    /// A `std::collections::SortedMap`, which never changes after it is made.
    SortedMap(Tree),
    /// A `std::collections::SortedSet`: a sorted map whose values are all `void`.
    SortedSet(Tree),
    /// A `std::task::Scope`.
    Scope(ScopeState),
    /// A `std::task::Task`.
    Task(TaskState),
}

/// What a `std::task::Scope` keeps track of.
#[derive(Debug, Default)]
pub struct ScopeState {
    /// How many of the tasks spawned in it haven't finished.
    pub live: usize,
    /// Whether its `scope` call has ended, so no more tasks can be spawned
    /// in it.
    pub ended: bool,
    /// The task waiting for the scope's tasks to finish, so it can end it.
    pub closer: Option<TaskId>,
    /// The task that opened it, whose `scope` call it is in.
    pub owner: TaskId,
    /// Whether it has been cancelled, so its tasks stop and any spawned in
    /// it from now on never start.
    pub cancelled: bool,
}

/// What a `std::task::Task` keeps track of.
#[derive(Debug, Default)]
pub struct TaskState {
    /// What the task returned, once it has finished.
    pub result: Option<Value>,
    /// The tasks waiting for it to finish.
    pub joiners: Vec<TaskId>,
    /// Whether it was cancelled, and so finished without a result.
    pub cancelled: bool,
}

impl Obj {
    /// Roughly how many bytes making the object took, for deciding when to
    /// collect. For a list, that is the nodes it doesn't share with another
    /// list (usually the one it was made from), since a new version of a
    /// list costs only the few nodes it changed.
    fn size(&self) -> usize {
        size_of::<Option<Obj>>()
            + match self {
                Obj::Closure(c) => c.upvalues.len() * size_of::<Value>(),
                Obj::Cell(_) => 0,
                Obj::Data(_, fields) => fields.len() * size_of::<Value>(),
                Obj::Str(s) => s.len(),
                Obj::List(l) => l.unshared_bytes(),
                Obj::Map(m) | Obj::Set(m) => m.unshared_bytes(),
                Obj::SortedMap(t) | Obj::SortedSet(t) => t.unshared_bytes(),
                Obj::Scope(_) => 0,
                Obj::Task(t) => t.joiners.len() * size_of::<TaskId>(),
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
    /// The bytes the objects that survived the last collection use, plus the
    /// [`Obj::size`] of each object allocated since.
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

    pub fn alloc_list(&mut self, list: List) -> Gc {
        self.alloc(Obj::List(list))
    }

    pub fn alloc_map(&mut self, map: Hamt) -> Gc {
        self.alloc(Obj::Map(map))
    }

    pub fn alloc_set(&mut self, set: Hamt) -> Gc {
        self.alloc(Obj::Set(set))
    }

    pub fn alloc_sorted_map(&mut self, tree: Tree) -> Gc {
        self.alloc(Obj::SortedMap(tree))
    }

    pub fn alloc_sorted_set(&mut self, tree: Tree) -> Gc {
        self.alloc(Obj::SortedSet(tree))
    }

    /// A new scope, opened by task `owner`.
    pub fn alloc_scope(&mut self, owner: TaskId) -> Gc {
        self.alloc(Obj::Scope(ScopeState {
            owner,
            ..ScopeState::default()
        }))
    }

    pub fn alloc_task(&mut self) -> Gc {
        self.alloc(Obj::Task(TaskState::default()))
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

    pub fn list(&self, gc: Gc) -> &List {
        match self.get(gc) {
            Obj::List(l) => l,
            _ => panic!("internal error: object {gc} is not a list"),
        }
    }

    /// The tree of a sorted map or a sorted set.
    pub fn tree(&self, gc: Gc) -> &Tree {
        match self.get(gc) {
            Obj::SortedMap(t) | Obj::SortedSet(t) => t,
            _ => panic!("internal error: object {gc} is not a sorted map or set"),
        }
    }

    /// The keys and values of a sorted map or set, in order: key, value,
    /// key, value, ...
    fn sorted_entries(&self, gc: Gc) -> Vec<Value> {
        let mut out = Vec::new();
        self.tree(gc).for_each(|k, v| {
            out.push(k.clone());
            out.push(v.clone());
        });
        out
    }

    /// The trie of a map or a set.
    pub fn hamt(&self, gc: Gc) -> &Hamt {
        match self.get(gc) {
            Obj::Map(m) | Obj::Set(m) => m,
            _ => panic!("internal error: object {gc} is not a map or set"),
        }
    }

    fn get_mut(&mut self, gc: Gc) -> &mut Obj {
        match &mut self.objects[gc.0 as usize] {
            Some(obj) => obj,
            None => panic!("internal error: object {gc} was used after being freed"),
        }
    }

    pub fn scope(&mut self, gc: Gc) -> &mut ScopeState {
        match self.get_mut(gc) {
            Obj::Scope(s) => s,
            _ => panic!("internal error: object {gc} is not a scope"),
        }
    }

    pub fn task(&mut self, gc: Gc) -> &mut TaskState {
        match self.get_mut(gc) {
            Obj::Task(t) => t,
            _ => panic!("internal error: object {gc} is not a task"),
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
                (Value::List(x), Value::List(y)) if x == y => true,
                (Value::List(x), Value::List(y)) => {
                    let (lx, ly) = (self.list(*x), self.list(*y));
                    if lx.len() != ly.len() {
                        return false;
                    }
                    work.extend(lx.iter().cloned().zip(ly.iter().cloned()));
                    true
                }
                (Value::Map(x), Value::Map(y)) | (Value::Set(x), Value::Set(y)) if x == y => true,
                (Value::Map(x), Value::Map(y)) | (Value::Set(x), Value::Set(y)) => {
                    // Equal maps can have differently shaped tries, so each
                    // key of one is looked up in the other.
                    let (mx, my) = (self.hamt(*x), self.hamt(*y));
                    if mx.len() != my.len() {
                        return false;
                    }
                    let mut pairs = Vec::new();
                    let mut all = true;
                    mx.for_each(|k, v| {
                        if !all {
                            return;
                        }
                        match my.get(self.hash(k), k, &|a, b| self.equal(a, b)) {
                            Some(w) => pairs.push((v.clone(), w.clone())),
                            None => all = false,
                        }
                    });
                    if !all {
                        return false;
                    }
                    work.extend(pairs);
                    true
                }
                (Value::SortedMap(x), Value::SortedMap(y))
                | (Value::SortedSet(x), Value::SortedSet(y))
                    if x == y =>
                {
                    true
                }
                // In key order, equal sorted maps have equal entries in
                // turn, whatever the shapes of their trees.
                (Value::SortedMap(x), Value::SortedMap(y))
                | (Value::SortedSet(x), Value::SortedSet(y)) => {
                    let (ex, ey) = (self.sorted_entries(*x), self.sorted_entries(*y));
                    if ex.len() != ey.len() {
                        return false;
                    }
                    work.extend(ex.into_iter().zip(ey));
                    true
                }
                (Value::Void, Value::Void) => true,
                // A scope or task is equal only to itself.
                (Value::Scope(x), Value::Scope(y)) | (Value::Task(x), Value::Task(y)) => x == y,
                // The type checker only allows `==` on types made of the above.
                _ => panic!("internal error: comparing {a:?} with {b:?}"),
            };
            if !same {
                return false;
            }
        }
        true
    }

    /// How two values of the same `Ord` type compare: ints and strings in
    /// their natural order (strings by code point), `false` before `true`,
    /// struct fields and tuple parts in turn, an enum's variants in the
    /// order they are declared and then their values, and lists value by
    /// value, a shorter list first if one is the start of the other.
    /// Iterative, so long lists can't overflow the stack.
    pub fn compare(&self, a: &Value, b: &Value) -> Ordering {
        enum Work {
            Values(Value, Value),
            /// After comparing the values two lists share, their lengths.
            Lengths(usize, usize),
        }
        let mut work = vec![Work::Values(a.clone(), b.clone())];
        while let Some(w) = work.pop() {
            let (a, b) = match w {
                Work::Lengths(x, y) => match x.cmp(&y) {
                    Ordering::Equal => continue,
                    o => return o,
                },
                Work::Values(a, b) => (a, b),
            };
            let order = match (&a, &b) {
                (Value::Data(x), Value::Data(y)) if x == y => Ordering::Equal,
                (Value::Data(x), Value::Data(y)) => {
                    let ((lx, fx), (ly, fy)) = (self.data(*x), self.data(*y));
                    match lx.tag.cmp(&ly.tag) {
                        Ordering::Equal => {
                            // Pushed last to first, so the first is compared first.
                            work.extend(
                                fx.iter()
                                    .cloned()
                                    .zip(fy.iter().cloned())
                                    .rev()
                                    .map(|(x, y)| Work::Values(x, y)),
                            );
                            Ordering::Equal
                        }
                        o => o,
                    }
                }
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
                (Value::Str(x), Value::Str(y)) if x == y => Ordering::Equal,
                // UTF-8 bytes sort in the order of the code points they encode.
                (Value::Str(x), Value::Str(y)) => self.str(*x).cmp(self.str(*y)),
                (Value::List(x), Value::List(y)) if x == y => Ordering::Equal,
                (Value::List(x), Value::List(y)) => {
                    let (lx, ly) = (self.list(*x), self.list(*y));
                    work.push(Work::Lengths(lx.len(), ly.len()));
                    let pairs: Vec<Work> = lx
                        .iter()
                        .cloned()
                        .zip(ly.iter().cloned())
                        .map(|(x, y)| Work::Values(x, y))
                        .collect();
                    work.extend(pairs.into_iter().rev());
                    Ordering::Equal
                }
                // Entry by entry in key order, like a list of pairs.
                (Value::SortedMap(x), Value::SortedMap(y))
                | (Value::SortedSet(x), Value::SortedSet(y)) => {
                    let (ex, ey) = (self.sorted_entries(*x), self.sorted_entries(*y));
                    work.push(Work::Lengths(ex.len(), ey.len()));
                    let pairs: Vec<Work> = ex
                        .into_iter()
                        .zip(ey)
                        .map(|(x, y)| Work::Values(x, y))
                        .collect();
                    work.extend(pairs.into_iter().rev());
                    Ordering::Equal
                }
                (Value::Void, Value::Void) => Ordering::Equal,
                // The type checker only allows ordering types made of the above.
                _ => panic!("internal error: ordering {a:?} and {b:?}"),
            };
            if order != Ordering::Equal {
                return order;
            }
        }
        Ordering::Equal
    }

    /// A hash of a value of a `Hash` type, consistent with [`Heap::equal`]:
    /// equal values hash the same. A map's or set's hash doesn't depend on
    /// the order of its entries, which equal maps needn't share. The hash
    /// is the same on every run of the same `aja` binary.
    pub fn hash(&self, value: &Value) -> u64 {
        let mut h = DefaultHasher::new();
        // An explicit worklist, so long lists can't overflow the stack.
        let mut work = vec![value.clone()];
        while let Some(v) = work.pop() {
            match &v {
                Value::Void => h.write_u8(0),
                Value::Int(i) => {
                    h.write_u8(1);
                    h.write_i64(*i);
                }
                Value::Bool(b) => {
                    h.write_u8(2);
                    h.write_u8(*b as u8);
                }
                Value::Str(gc) => {
                    h.write_u8(3);
                    self.str(*gc).hash(&mut h);
                }
                Value::Data(gc) => {
                    let (layout, fields) = self.data(*gc);
                    h.write_u8(4);
                    h.write_u32(layout.tag);
                    work.extend(fields.iter().rev().cloned());
                }
                Value::List(gc) => {
                    let list = self.list(*gc);
                    h.write_u8(5);
                    h.write_usize(list.len());
                    let values: Vec<Value> = list.iter().cloned().collect();
                    work.extend(values.into_iter().rev());
                }
                Value::Map(gc) | Value::Set(gc) => {
                    let m = self.hamt(*gc);
                    h.write_u8(if matches!(v, Value::Map(_)) { 6 } else { 7 });
                    h.write_usize(m.len());
                    // A sum of the entries' hashes, which any order gives.
                    let mut sum = 0u64;
                    m.for_each(|k, v| {
                        let mut e = DefaultHasher::new();
                        e.write_u64(self.hash(k));
                        e.write_u64(self.hash(v));
                        sum = sum.wrapping_add(e.finish());
                    });
                    h.write_u64(sum);
                }
                Value::SortedMap(gc) | Value::SortedSet(gc) => {
                    let entries = self.sorted_entries(*gc);
                    h.write_u8(if matches!(v, Value::SortedMap(_)) {
                        8
                    } else {
                        9
                    });
                    h.write_usize(entries.len());
                    work.extend(entries.into_iter().rev());
                }
                // The type checker only allows hashing types made of the above.
                _ => panic!("internal error: hashing {v:?}"),
            }
        }
        h.finish()
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
        // What the reachable objects use, counting each list node once
        // however many versions of a list share it.
        let mut live_bytes = 0;
        let mut seen_nodes = HashSet::new();
        while let Some(gc) = work.pop() {
            let i = gc.0 as usize;
            // A register in a frame's window that the frame hasn't written
            // yet may still hold a handle an earlier call left there, to an
            // object freed since. The frame never reads it, so it is no
            // reason to keep anything; if its slot has been reused, keeping
            // that object a while longer does no harm.
            if self.marks[i] || self.objects[i].is_none() {
                continue;
            }
            self.marks[i] = true;
            let obj = self.objects[i].as_ref().expect("checked above");
            live_bytes += match obj {
                Obj::List(l) => {
                    size_of::<Option<Obj>>()
                        + l.visit_unseen(&mut seen_nodes, |v| work.extend(v.handle()))
                }
                Obj::Map(m) | Obj::Set(m) => {
                    size_of::<Option<Obj>>()
                        + m.visit_unseen(&mut seen_nodes, |v| work.extend(v.handle()))
                }
                Obj::SortedMap(t) | Obj::SortedSet(t) => {
                    size_of::<Option<Obj>>()
                        + t.visit_unseen(&mut seen_nodes, |v| work.extend(v.handle()))
                }
                other => other.size(),
            };
            match obj {
                Obj::Closure(c) => work.extend(c.upvalues.iter().filter_map(Value::handle)),
                Obj::Cell(v) => work.extend(v.handle()),
                Obj::Data(_, fields) => work.extend(fields.iter().filter_map(Value::handle)),
                Obj::Str(_)
                | Obj::Scope(_)
                | Obj::List(_)
                | Obj::Map(_)
                | Obj::Set(_)
                | Obj::SortedMap(_)
                | Obj::SortedSet(_) => {}
                Obj::Task(t) => work.extend(t.result.as_ref().and_then(Value::handle)),
            }
        }

        // Sweep.
        for (i, (obj, marked)) in self.objects.iter_mut().zip(&mut self.marks).enumerate() {
            if !*marked {
                if obj.take().is_some() {
                    self.free.push(i as u32);
                    self.live -= 1;
                }
            }
            *marked = false;
        }
        // Collect again once as much has been allocated as survived, so the
        // time spent collecting stays proportional to the time allocating.
        self.bytes = live_bytes;
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
                    Value::List(gc) => {
                        let list = self.list(*gc);
                        let mut items = vec![Item::Text("[")];
                        for (i, v) in list.iter().enumerate() {
                            if i > 0 {
                                items.push(Item::Text(", "));
                            }
                            items.push(Item::Value(v, true));
                        }
                        items.push(Item::Text("]"));
                        stack.extend(items.into_iter().rev());
                        continue;
                    }
                    Value::Map(gc) | Value::Set(gc) => {
                        let is_map = matches!(v, Value::Map(_));
                        let mut items = vec![Item::Text("{")];
                        let mut first = true;
                        self.hamt(*gc).for_each(|k, v| {
                            if !first {
                                items.push(Item::Text(", "));
                            }
                            first = false;
                            items.push(Item::Value(k, true));
                            if is_map {
                                items.push(Item::Text(": "));
                                items.push(Item::Value(v, true));
                            }
                        });
                        items.push(Item::Text("}"));
                        stack.extend(items.into_iter().rev());
                        continue;
                    }
                    Value::SortedMap(gc) | Value::SortedSet(gc) => {
                        let is_map = matches!(v, Value::SortedMap(_));
                        let mut items = vec![Item::Text("{")];
                        let mut first = true;
                        self.tree(*gc).for_each(|k, v| {
                            if !first {
                                items.push(Item::Text(", "));
                            }
                            first = false;
                            items.push(Item::Value(k, true));
                            if is_map {
                                items.push(Item::Text(": "));
                                items.push(Item::Value(v, true));
                            }
                        });
                        items.push(Item::Text("}"));
                        stack.extend(items.into_iter().rev());
                        continue;
                    }
                    Value::Void => "void".to_string(),
                    Value::Int(i) => i.to_string(),
                    Value::Float(n) => format!("{n:?}"),
                    Value::Bool(b) => b.to_string(),
                    Value::Str(gc) => {
                        out.push_str(self.str(*gc));
                        continue;
                    }
                    Value::Fn(gc) => format!("<fn {}>", self.closure(*gc).proto.signature),
                    Value::Native(name, _) => format!("<native fn: {name}>"),
                    Value::Runtime(f) => format!("<native fn: {}>", f.name()),
                    Value::Scope(_) => "<scope>".to_string(),
                    Value::Task(_) => "<task>".to_string(),
                },
            };
            out.push_str(&value);
        }
        out
    }
}
