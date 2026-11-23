//! The VM's lists: persistent vectors, as in Clojure.
//!
//! A list is never changed once made; `push`, `set`, and `pop` make a new
//! list that shares most of its structure with the old one. The values are
//! kept in a tree with 32 children per node, and the last (up to) 32 in a
//! separate tail, so reading or replacing a value costs O(log₃₂ n) (at most
//! 7 steps for any list that fits in memory) and adding one to the end is
//! usually just a push onto the tail.
//!
//! Nodes are reference-counted and changed through [`Rc::make_mut`], so a
//! node only this list uses is changed in place, and a shared one is copied
//! first. Building a list from scratch therefore copies nothing.

use std::rc::Rc;

use super::value::Value;

const BITS: u32 = 5;
const WIDTH: usize = 1 << BITS;
const MASK: usize = WIDTH - 1;

#[derive(Clone)]
enum Node {
    Branch(Vec<Rc<Node>>),
    Leaf(Vec<Value>),
}

impl Node {
    fn children(&self) -> &Vec<Rc<Node>> {
        match self {
            Node::Branch(c) => c,
            Node::Leaf(_) => unreachable!("a branch was expected"),
        }
    }

    fn children_mut(&mut self) -> &mut Vec<Rc<Node>> {
        match self {
            Node::Branch(c) => c,
            Node::Leaf(_) => unreachable!("a branch was expected"),
        }
    }

    fn values(&self) -> &Vec<Value> {
        match self {
            Node::Leaf(v) => v,
            Node::Branch(_) => unreachable!("a leaf was expected"),
        }
    }

    fn values_mut(&mut self) -> &mut Vec<Value> {
        match self {
            Node::Leaf(v) => v,
            Node::Branch(_) => unreachable!("a leaf was expected"),
        }
    }
}

#[derive(Clone)]
pub struct List {
    len: usize,
    /// How far to shift an index to find its child of the root.
    shift: u32,
    root: Rc<Node>,
    tail: Rc<Vec<Value>>,
}

impl Default for List {
    fn default() -> Self {
        List {
            len: 0,
            shift: BITS,
            root: Rc::new(Node::Branch(Vec::new())),
            tail: Rc::new(Vec::new()),
        }
    }
}

impl FromIterator<Value> for List {
    fn from_iter<I: IntoIterator<Item = Value>>(values: I) -> Self {
        let mut list = List::default();
        for v in values {
            list.push(v);
        }
        list
    }
}

impl List {
    pub fn len(&self) -> usize {
        self.len
    }

    /// The index of the first value in the tail.
    fn tail_offset(&self) -> usize {
        if self.len < WIDTH {
            0
        } else {
            ((self.len - 1) >> BITS) << BITS
        }
    }

    /// The leaf that holds index `i`, which is in the tree, not the tail.
    fn leaf(&self, i: usize) -> &Vec<Value> {
        let mut node = &self.root;
        let mut level = self.shift;
        while level > 0 {
            node = &node.children()[(i >> level) & MASK];
            level -= BITS;
        }
        node.values()
    }

    pub fn get(&self, i: usize) -> Option<&Value> {
        if i >= self.len {
            None
        } else if i >= self.tail_offset() {
            Some(&self.tail[i & MASK])
        } else {
            Some(&self.leaf(i)[i & MASK])
        }
    }

    pub fn push(&mut self, value: Value) {
        if self.len - self.tail_offset() < WIDTH {
            Rc::make_mut(&mut self.tail).push(value);
            self.len += 1;
            return;
        }
        // The tail is full: move it into the tree.
        let full = std::mem::replace(&mut self.tail, Rc::new(vec![value]));
        let leaf = Rc::new(Node::Leaf(Rc::unwrap_or_clone(full)));
        if (self.len >> BITS) > (1 << self.shift) {
            // The tree is full too: add a level.
            let old = std::mem::replace(&mut self.root, Rc::new(Node::Branch(Vec::new())));
            let path = new_path(self.shift, leaf);
            *Rc::make_mut(&mut self.root).children_mut() = vec![old, path];
            self.shift += BITS;
        } else {
            push_tail(self.len, self.shift, &mut self.root, leaf);
        }
        self.len += 1;
    }

    /// Replace the value at `i`, which must be in range.
    pub fn set(&mut self, i: usize, value: Value) {
        assert!(i < self.len, "index in range");
        if i >= self.tail_offset() {
            Rc::make_mut(&mut self.tail)[i & MASK] = value;
            return;
        }
        let mut node = Rc::make_mut(&mut self.root);
        let mut level = self.shift;
        while level > 0 {
            let child = &mut node.children_mut()[(i >> level) & MASK];
            node = Rc::make_mut(child);
            level -= BITS;
        }
        node.values_mut()[i & MASK] = value;
    }

    /// Remove the last value, if there is one.
    pub fn pop(&mut self) {
        match self.len {
            0 => return,
            1 => {
                *self = List::default();
                return;
            }
            _ => {}
        }
        if self.len - self.tail_offset() > 1 {
            Rc::make_mut(&mut self.tail).pop();
            self.len -= 1;
            return;
        }
        // The tail empties: the last leaf of the tree becomes the tail.
        let new_tail = self.leaf(self.len - 2).clone();
        let root = pop_tail(self.len, self.shift, &self.root)
            .unwrap_or_else(|| Rc::new(Node::Branch(Vec::new())));
        self.root = root;
        if self.shift > BITS && self.root.children().len() == 1 {
            self.root = self.root.children()[0].clone();
            self.shift -= BITS;
        }
        self.tail = Rc::new(new_tail);
        self.len -= 1;
    }

    /// Every value, in order.
    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        (0..self.len).map(|i| self.get(i).expect("index in range"))
    }

    /// Call `f` with every value, faster than [`List::iter`].
    pub fn for_each(&self, mut f: impl FnMut(&Value)) {
        fn visit(node: &Node, f: &mut impl FnMut(&Value)) {
            match node {
                Node::Branch(children) => children.iter().for_each(|c| visit(c, f)),
                Node::Leaf(values) => values.iter().for_each(&mut *f),
            }
        }
        visit(&self.root, &mut f);
        self.tail.iter().for_each(f);
    }
}

/// A chain of branches down to `leaf`, from a node at `level`.
fn new_path(level: u32, leaf: Rc<Node>) -> Rc<Node> {
    if level == 0 {
        leaf
    } else {
        Rc::new(Node::Branch(vec![new_path(level - BITS, leaf)]))
    }
}

/// Add `leaf` to the tree under `parent` (at `level`), for a list of `len`
/// values whose full tail `leaf` was.
fn push_tail(len: usize, level: u32, parent: &mut Rc<Node>, leaf: Rc<Node>) {
    let index = ((len - 1) >> level) & MASK;
    let children = Rc::make_mut(parent).children_mut();
    if level == BITS {
        children.push(leaf);
    } else if index < children.len() {
        push_tail(len, level - BITS, &mut children[index], leaf);
    } else {
        children.push(new_path(level - BITS, leaf));
    }
}

/// `node` (at `level`) without the tree's last leaf, for a list of `len`
/// values, or `None` if nothing would be left.
fn pop_tail(len: usize, level: u32, node: &Rc<Node>) -> Option<Rc<Node>> {
    let index = ((len - 2) >> level) & MASK;
    let children = node.children();
    if level > BITS {
        let child = pop_tail(len, level - BITS, &children[index]);
        if child.is_none() && index == 0 {
            return None;
        }
        let mut kept = children[..index].to_vec();
        kept.extend(child);
        Some(Rc::new(Node::Branch(kept)))
    } else if index == 0 {
        None
    } else {
        Some(Rc::new(Node::Branch(children[..index].to_vec())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int(v: &Value) -> i64 {
        match v {
            Value::Int(i) => *i,
            _ => panic!("not an int"),
        }
    }

    fn ints(list: &List) -> Vec<i64> {
        let mut out = Vec::new();
        list.for_each(|v| out.push(int(v)));
        assert_eq!(out, list.iter().map(int).collect::<Vec<_>>());
        out
    }

    #[test]
    fn grows_and_shrinks_through_every_level() {
        // Past one leaf, one full branch (32 * 32), and into a third level.
        let n = 32 * 32 * 2 + 70;
        let mut list = List::default();
        for i in 0..n {
            list.push(Value::Int(i));
            assert_eq!(list.len(), i as usize + 1);
        }
        assert_eq!(ints(&list), (0..n).collect::<Vec<_>>());
        for i in (0..n).rev() {
            assert_eq!(int(list.get(i as usize).unwrap()), i);
            list.pop();
            assert_eq!(list.len(), i as usize);
            if i % 97 == 0 {
                assert_eq!(ints(&list), (0..i).collect::<Vec<_>>());
            }
        }
        assert!(list.get(0).is_none());
        list.pop();
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn versions_are_independent() {
        let base: List = (0..1000).map(Value::Int).collect();
        let mut changed = base.clone();
        changed.set(5, Value::Int(-1));
        changed.set(999, Value::Int(-2));
        changed.push(Value::Int(1000));
        let mut shorter = base.clone();
        for _ in 0..40 {
            shorter.pop();
        }
        assert_eq!(ints(&base), (0..1000).collect::<Vec<_>>());
        assert_eq!(int(changed.get(5).unwrap()), -1);
        assert_eq!(int(changed.get(999).unwrap()), -2);
        assert_eq!(changed.len(), 1001);
        assert_eq!(ints(&shorter), (0..960).collect::<Vec<_>>());
    }
}
