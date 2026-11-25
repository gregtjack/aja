//! The VM's maps and sets: hash array mapped tries (HAMTs), as in Clojure.
//!
//! Like a list, a map never changes once made: `insert` and `remove` make a
//! new map that shares all but one path of nodes with the old one. Each
//! level of the trie uses 5 bits of a key's 64-bit hash to pick one of up to
//! 32 children, and a bitmap says which are present, so a node holds only
//! the children it has. A lookup takes at most 13 steps, and usually 3 or 4.
//! Keys whose hashes are equal in every bit share a collision node.
//!
//! The trie doesn't know how to hash or compare values, which depends on the
//! heap they live in; callers pass a key's hash and a function for equality.
//! A set is a map whose values are all `void`.
//!
//! Nodes are reference-counted and changed through [`Rc::make_mut`], so a
//! node only this map uses is changed in place, and a shared one is copied.

use std::{collections::HashSet, mem::size_of, rc::Rc};

use super::value::Value;

const BITS: u32 = 5;
const MASK: u64 = (1 << BITS) - 1;

#[derive(Clone)]
enum Node {
    /// The children present, in order of the 5 bits of hash that pick them;
    /// bit `i` of the bitmap is set if child `i` is.
    Branch(u32, Vec<Child>),
    /// Two or more entries whose keys have this same hash.
    Collision(u64, Vec<(Value, Value)>),
}

#[derive(Clone)]
enum Child {
    /// A key's hash, the key, and its value.
    Entry(u64, Value, Value),
    Node(Rc<Node>),
}

/// A persistent map from values to values.
#[derive(Clone)]
pub struct Hamt {
    len: usize,
    root: Rc<Node>,
}

impl Default for Hamt {
    fn default() -> Self {
        Hamt {
            len: 0,
            root: Rc::new(Node::Branch(0, Vec::new())),
        }
    }
}

/// Which of a branch's 32 children `hash` goes to at `shift`, as a bit.
fn bit(hash: u64, shift: u32) -> u32 {
    1 << ((hash >> shift) & MASK)
}

/// Where the child with `bit` is (or would go) among a branch's children.
fn index(bitmap: u32, bit: u32) -> usize {
    (bitmap & (bit - 1)).count_ones() as usize
}

impl Hamt {
    pub fn len(&self) -> usize {
        self.len
    }

    /// The value for `key`, whose hash is `hash`, using `eq` to compare keys.
    pub fn get(
        &self,
        hash: u64,
        key: &Value,
        eq: &dyn Fn(&Value, &Value) -> bool,
    ) -> Option<&Value> {
        let mut node = &self.root;
        let mut shift = 0;
        loop {
            match &**node {
                Node::Branch(bitmap, children) => {
                    let b = bit(hash, shift);
                    if bitmap & b == 0 {
                        return None;
                    }
                    match &children[index(*bitmap, b)] {
                        Child::Entry(h, k, v) => {
                            return (*h == hash && eq(k, key)).then_some(v);
                        }
                        Child::Node(n) => {
                            node = n;
                            shift += BITS;
                        }
                    }
                }
                Node::Collision(h, entries) => {
                    if *h != hash {
                        return None;
                    }
                    return entries.iter().find(|(k, _)| eq(k, key)).map(|(_, v)| v);
                }
            }
        }
    }

    /// Set `key`'s value to `value`, adding the key if it isn't there.
    pub fn insert(
        &mut self,
        hash: u64,
        key: Value,
        value: Value,
        eq: &dyn Fn(&Value, &Value) -> bool,
    ) {
        if insert(&mut self.root, 0, hash, key, value, eq) {
            self.len += 1;
        }
    }

    /// Remove `key`, if it is there. Returns whether it was.
    pub fn remove(&mut self, hash: u64, key: &Value, eq: &dyn Fn(&Value, &Value) -> bool) -> bool {
        // Look first, so removing a missing key copies nothing.
        if self.get(hash, key, eq).is_none() {
            return false;
        }
        remove(&mut self.root, 0, hash, key, eq);
        self.len -= 1;
        true
    }

    /// Call `f` with every key and its value, in an order that depends only
    /// on the keys' hashes (and, for keys with equal hashes, the order they
    /// were added).
    pub fn for_each<'a>(&'a self, mut f: impl FnMut(&'a Value, &'a Value)) {
        fn visit<'a>(node: &'a Node, f: &mut impl FnMut(&'a Value, &'a Value)) {
            match node {
                Node::Branch(_, children) => {
                    for c in children {
                        match c {
                            Child::Entry(_, k, v) => f(k, v),
                            Child::Node(n) => visit(n, f),
                        }
                    }
                }
                Node::Collision(_, entries) => entries.iter().for_each(|(k, v)| f(k, v)),
            }
        }
        visit(&self.root, &mut f);
    }

    /// The bytes of the nodes no other map shares (see `List::unshared_bytes`).
    pub fn unshared_bytes(&self) -> usize {
        fn node(n: &Rc<Node>) -> usize {
            if Rc::strong_count(n) > 1 {
                return 0;
            }
            own_bytes(n) + children(n).map(node).sum::<usize>()
        }
        node(&self.root)
    }

    /// Call `f` with the keys and values in this map's nodes that aren't in
    /// `seen`, adding them to it, and return the bytes those nodes use (see
    /// `List::visit_unseen`).
    pub fn visit_unseen(&self, seen: &mut HashSet<usize>, mut f: impl FnMut(&Value)) -> usize {
        fn visit(n: &Rc<Node>, seen: &mut HashSet<usize>, f: &mut impl FnMut(&Value)) -> usize {
            if !seen.insert(Rc::as_ptr(n) as usize) {
                return 0;
            }
            match &**n {
                Node::Branch(_, cs) => {
                    for c in cs {
                        if let Child::Entry(_, k, v) = c {
                            f(k);
                            f(v);
                        }
                    }
                }
                Node::Collision(_, entries) => {
                    for (k, v) in entries {
                        f(k);
                        f(v);
                    }
                }
            }
            own_bytes(n) + children(n).map(|c| visit(c, seen, f)).sum::<usize>()
        }
        visit(&self.root, seen, &mut f)
    }
}

/// The bytes a node itself uses, besides its child nodes.
fn own_bytes(n: &Node) -> usize {
    match n {
        Node::Branch(_, cs) => cs.len() * size_of::<Child>(),
        Node::Collision(_, es) => es.len() * size_of::<(Value, Value)>(),
    }
}

/// A node's child nodes.
fn children(n: &Node) -> impl Iterator<Item = &Rc<Node>> {
    let cs: &[Child] = match n {
        Node::Branch(_, cs) => cs,
        Node::Collision(..) => &[],
    };
    cs.iter().filter_map(|c| match c {
        Child::Node(n) => Some(n),
        Child::Entry(..) => None,
    })
}

/// Insert into `node`, at `shift` bits into the hash. Returns whether the
/// key is new.
fn insert(
    node: &mut Rc<Node>,
    shift: u32,
    hash: u64,
    key: Value,
    value: Value,
    eq: &dyn Fn(&Value, &Value) -> bool,
) -> bool {
    if let Node::Collision(h, _) = &**node {
        let h = *h;
        if h != hash {
            // A key with another hash shares this node's hash up to here:
            // put the collision node in a branch, which tells them apart.
            let collision = node.clone();
            *node = Rc::new(Node::Branch(bit(h, shift), vec![Child::Node(collision)]));
        }
    }
    match Rc::make_mut(node) {
        Node::Branch(bitmap, children) => {
            let b = bit(hash, shift);
            let i = index(*bitmap, b);
            if *bitmap & b == 0 {
                children.insert(i, Child::Entry(hash, key, value));
                *bitmap |= b;
                return true;
            }
            match &mut children[i] {
                Child::Entry(h, k, v) if *h == hash && eq(k, &key) => {
                    *v = value;
                    false
                }
                Child::Entry(..) => {
                    let Child::Entry(h, k, v) = std::mem::replace(
                        &mut children[i],
                        Child::Entry(0, Value::Void, Value::Void),
                    ) else {
                        unreachable!()
                    };
                    children[i] = Child::Node(two(shift + BITS, (h, k, v), (hash, key, value)));
                    true
                }
                Child::Node(n) => insert(n, shift + BITS, hash, key, value, eq),
            }
        }
        Node::Collision(_, entries) => match entries.iter_mut().find(|(k, _)| eq(k, &key)) {
            Some((_, v)) => {
                *v = value;
                false
            }
            None => {
                entries.push((key, value));
                true
            }
        },
    }
}

/// A node holding two entries with different keys, at `shift` bits into
/// the hash.
fn two(shift: u32, a: (u64, Value, Value), b: (u64, Value, Value)) -> Rc<Node> {
    if a.0 == b.0 {
        return Rc::new(Node::Collision(a.0, vec![(a.1, a.2), (b.1, b.2)]));
    }
    let (ba, bb) = (bit(a.0, shift), bit(b.0, shift));
    if ba == bb {
        // The same child here: they part further down.
        return Rc::new(Node::Branch(ba, vec![Child::Node(two(shift + BITS, a, b))]));
    }
    let (ea, eb) = (Child::Entry(a.0, a.1, a.2), Child::Entry(b.0, b.1, b.2));
    let children = if ba < bb { vec![ea, eb] } else { vec![eb, ea] };
    Rc::new(Node::Branch(ba | bb, children))
}

/// Remove `key`, which is in `node`, at `shift` bits into the hash. A node
/// left with a single entry is replaced by the entry in its parent, so maps
/// don't keep long chains after their keys are removed.
fn remove(
    node: &mut Rc<Node>,
    shift: u32,
    hash: u64,
    key: &Value,
    eq: &dyn Fn(&Value, &Value) -> bool,
) {
    match Rc::make_mut(node) {
        Node::Branch(bitmap, children) => {
            let b = bit(hash, shift);
            let i = index(*bitmap, b);
            match &mut children[i] {
                Child::Entry(..) => {
                    children.remove(i);
                    *bitmap &= !b;
                }
                Child::Node(n) => {
                    remove(n, shift + BITS, hash, key, eq);
                    let lifted = match &**n {
                        Node::Branch(_, cs)
                            if cs.len() == 1 && matches!(cs[0], Child::Entry(..)) =>
                        {
                            Some(cs[0].clone())
                        }
                        Node::Collision(h, es) if es.len() == 1 => {
                            Some(Child::Entry(*h, es[0].0.clone(), es[0].1.clone()))
                        }
                        _ => None,
                    };
                    if let Some(entry) = lifted {
                        children[i] = entry;
                    }
                }
            }
        }
        Node::Collision(_, entries) => {
            if let Some(i) = entries.iter().position(|(k, _)| eq(k, key)) {
                entries.remove(i);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eq(a: &Value, b: &Value) -> bool {
        matches!((a, b), (Value::Int(x), Value::Int(y)) if x == y)
    }

    fn int(v: &Value) -> i64 {
        match v {
            Value::Int(i) => *i,
            _ => panic!("not an int"),
        }
    }

    /// A deliberately poor hash, so some keys collide in every bit and
    /// others share long prefixes.
    fn hash(k: i64) -> u64 {
        if k % 7 == 0 {
            42
        } else {
            (k as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15) >> (k % 3 * 20)
        }
    }

    fn get(m: &Hamt, k: i64) -> Option<i64> {
        m.get(hash(k), &Value::Int(k), &eq).map(int)
    }

    fn entries(m: &Hamt) -> Vec<(i64, i64)> {
        let mut out = Vec::new();
        m.for_each(|k, v| out.push((int(k), int(v))));
        out.sort();
        out
    }

    #[test]
    fn inserts_finds_and_removes_through_collisions() {
        let mut m = Hamt::default();
        for k in 0..2000 {
            m.insert(hash(k), Value::Int(k), Value::Int(k * 10), &eq);
        }
        assert_eq!(m.len(), 2000);
        for k in 0..2000 {
            assert_eq!(get(&m, k), Some(k * 10), "key {k}");
        }
        assert_eq!(get(&m, 2000), None);
        assert_eq!(get(&m, -7), None); // collides with the multiples of 7
                                       // Replacing keeps the length.
        m.insert(hash(5), Value::Int(5), Value::Int(-5), &eq);
        assert_eq!((m.len(), get(&m, 5)), (2000, Some(-5)));

        let before = m.clone();
        for k in (0..2000).filter(|k| k % 2 == 0) {
            assert!(m.remove(hash(k), &Value::Int(k), &eq));
        }
        assert!(!m.remove(hash(0), &Value::Int(0), &eq));
        assert_eq!(m.len(), 1000);
        for k in 0..2000 {
            let want = if k % 2 == 0 {
                None
            } else if k == 5 {
                Some(-5)
            } else {
                Some(k * 10)
            };
            assert_eq!(get(&m, k), want, "key {k}");
        }
        // The old version is untouched.
        assert_eq!(entries(&before).len(), 2000);
        assert_eq!(get(&before, 4), Some(40));
        for k in 0..2000 {
            m.remove(hash(k), &Value::Int(k), &eq);
        }
        assert_eq!((m.len(), entries(&m)), (0, vec![]));
    }

    #[test]
    fn versions_count_only_what_they_dont_share() {
        let mut m = Hamt::default();
        for k in 0..5000 {
            m.insert(hash(k), Value::Int(k), Value::Int(k), &eq);
        }
        let all = m.unshared_bytes();
        let mut changed = m.clone();
        changed.insert(hash(1), Value::Int(1), Value::Int(0), &eq);
        let path = changed.unshared_bytes();
        assert!(path * 20 < all, "{path} of {all}");
        let mut seen = HashSet::new();
        let (mut a, mut b) = (0, 0);
        assert_eq!(m.visit_unseen(&mut seen, |_| a += 1), all);
        assert!(changed.visit_unseen(&mut seen, |_| b += 1) <= path);
        assert_eq!(a, 10_000);
        assert!(b < 200, "{b}");
    }
}
