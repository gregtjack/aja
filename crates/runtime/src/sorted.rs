//! The VM's sorted maps and sets: weight-balanced binary trees, as in
//! Haskell's `Data.Map` (Adams, "Efficient sets: a balancing act", with the
//! parameters Hirai and Yamamoto proved correct).
//!
//! A tree never changes once made: `insert` and `remove` build new nodes
//! along the path to the key and share every other node with the old tree.
//! Each node knows its size, and neither side of a node is ever more than
//! three times the size of the other, so the tree's depth stays under
//! about 2 log₂ n and every operation takes O(log n) steps.
//!
//! Like the hash trie, the tree doesn't know how to compare values, which
//! depends on the heap they live in; callers pass the comparison. A sorted
//! set is a sorted map whose values are all `void`.

use std::{cmp::Ordering, collections::HashSet, mem::size_of, rc::Rc};

use super::value::Value;

/// How much bigger one side may be than the other (Adams' ∆).
const DELTA: usize = 3;
/// When rebalancing, whether one rotation is enough or two are needed (Γ).
const RATIO: usize = 2;

type Link = Option<Rc<Node>>;

struct Node {
    size: usize,
    key: Value,
    value: Value,
    left: Link,
    right: Link,
}

/// A comparison of two keys.
pub type Cmp<'a> = &'a dyn Fn(&Value, &Value) -> Ordering;

/// A persistent map from values to values, kept in order of its keys.
#[derive(Clone, Default)]
pub struct Tree {
    root: Link,
}

fn size(t: &Link) -> usize {
    t.as_ref().map_or(0, |n| n.size)
}

fn node(key: Value, value: Value, left: Link, right: Link) -> Link {
    Some(Rc::new(Node {
        size: size(&left) + size(&right) + 1,
        key,
        value,
        left,
        right,
    }))
}

/// A node with `left` and `right`, which were balanced until one of them
/// gained or lost a key, rotated back into balance.
fn balance(key: Value, value: Value, left: Link, right: Link) -> Link {
    let (sl, sr) = (size(&left), size(&right));
    if sl + sr <= 1 {
        node(key, value, left, right)
    } else if sr > DELTA * sl {
        let r = right.expect("the right side is the bigger");
        if size(&r.left) < RATIO * size(&r.right) {
            // Single rotation to the left.
            node(
                r.key.clone(),
                r.value.clone(),
                node(key, value, left, r.left.clone()),
                r.right.clone(),
            )
        } else {
            // Double rotation: the right child's left child comes to the top.
            let rl = r.left.as_ref().expect("the inner side is the bigger");
            node(
                rl.key.clone(),
                rl.value.clone(),
                node(key, value, left, rl.left.clone()),
                node(
                    r.key.clone(),
                    r.value.clone(),
                    rl.right.clone(),
                    r.right.clone(),
                ),
            )
        }
    } else if sl > DELTA * sr {
        let l = left.expect("the left side is the bigger");
        if size(&l.right) < RATIO * size(&l.left) {
            node(
                l.key.clone(),
                l.value.clone(),
                l.left.clone(),
                node(key, value, l.right.clone(), right),
            )
        } else {
            let lr = l.right.as_ref().expect("the inner side is the bigger");
            node(
                lr.key.clone(),
                lr.value.clone(),
                node(
                    l.key.clone(),
                    l.value.clone(),
                    l.left.clone(),
                    lr.left.clone(),
                ),
                node(key, value, lr.right.clone(), right),
            )
        }
    } else {
        node(key, value, left, right)
    }
}

fn insert(t: &Link, key: Value, value: Value, cmp: Cmp) -> Link {
    let Some(n) = t else {
        return node(key, value, None, None);
    };
    match cmp(&key, &n.key) {
        Ordering::Less => balance(
            n.key.clone(),
            n.value.clone(),
            insert(&n.left, key, value, cmp),
            n.right.clone(),
        ),
        Ordering::Greater => balance(
            n.key.clone(),
            n.value.clone(),
            n.left.clone(),
            insert(&n.right, key, value, cmp),
        ),
        Ordering::Equal => node(key, value, n.left.clone(), n.right.clone()),
    }
}

/// `t` without `key`, which it has.
fn remove(t: &Link, key: &Value, cmp: Cmp) -> Link {
    let n = t.as_ref().expect("the key is in the tree");
    match cmp(key, &n.key) {
        Ordering::Less => balance(
            n.key.clone(),
            n.value.clone(),
            remove(&n.left, key, cmp),
            n.right.clone(),
        ),
        Ordering::Greater => balance(
            n.key.clone(),
            n.value.clone(),
            n.left.clone(),
            remove(&n.right, key, cmp),
        ),
        Ordering::Equal => glue(&n.left, &n.right),
    }
}

/// The keys of `left` and then `right`, which were the two sides of one
/// balanced node: the bigger side gives up its key nearest the other.
fn glue(left: &Link, right: &Link) -> Link {
    match (left, right) {
        (None, r) => r.clone(),
        (l, None) => l.clone(),
        (Some(l), Some(r)) if l.size > r.size => {
            let (k, v, rest) = remove_last(l);
            balance(k, v, rest, right.clone())
        }
        (Some(_), Some(r)) => {
            let (k, v, rest) = remove_first(r);
            balance(k, v, left.clone(), rest)
        }
    }
}

fn remove_first(n: &Rc<Node>) -> (Value, Value, Link) {
    match &n.left {
        None => (n.key.clone(), n.value.clone(), n.right.clone()),
        Some(l) => {
            let (k, v, rest) = remove_first(l);
            (
                k,
                v,
                balance(n.key.clone(), n.value.clone(), rest, n.right.clone()),
            )
        }
    }
}

fn remove_last(n: &Rc<Node>) -> (Value, Value, Link) {
    match &n.right {
        None => (n.key.clone(), n.value.clone(), n.left.clone()),
        Some(r) => {
            let (k, v, rest) = remove_last(r);
            (
                k,
                v,
                balance(n.key.clone(), n.value.clone(), n.left.clone(), rest),
            )
        }
    }
}

impl Tree {
    pub fn len(&self) -> usize {
        size(&self.root)
    }

    /// The value for `key`.
    pub fn get(&self, key: &Value, cmp: Cmp) -> Option<&Value> {
        let mut t = &self.root;
        while let Some(n) = t {
            t = match cmp(key, &n.key) {
                Ordering::Less => &n.left,
                Ordering::Greater => &n.right,
                Ordering::Equal => return Some(&n.value),
            };
        }
        None
    }

    /// Set `key`'s value to `value`, adding the key if it isn't there.
    pub fn insert(&mut self, key: Value, value: Value, cmp: Cmp) {
        self.root = insert(&self.root, key, value, cmp);
    }

    /// Remove `key`, if it is there. Returns whether it was.
    pub fn remove(&mut self, key: &Value, cmp: Cmp) -> bool {
        // Look first, so removing a missing key copies nothing.
        if self.get(key, cmp).is_none() {
            return false;
        }
        self.root = remove(&self.root, key, cmp);
        true
    }

    /// The entry with the smallest key.
    pub fn first(&self) -> Option<(&Value, &Value)> {
        let mut n = self.root.as_ref()?;
        while let Some(l) = &n.left {
            n = l;
        }
        Some((&n.key, &n.value))
    }

    /// The entry with the largest key.
    pub fn last(&self) -> Option<(&Value, &Value)> {
        let mut n = self.root.as_ref()?;
        while let Some(r) = &n.right {
            n = r;
        }
        Some((&n.key, &n.value))
    }

    /// Call `f` with every key and its value, in order of the keys.
    pub fn for_each<'a>(&'a self, mut f: impl FnMut(&'a Value, &'a Value)) {
        self.for_range(None, None, &|_, _| Ordering::Equal, &mut f);
    }

    /// Call `f`, in order, with the entries whose keys are at least `lo` and
    /// less than `hi` (where given).
    pub fn for_range<'a>(
        &'a self,
        lo: Option<&Value>,
        hi: Option<&Value>,
        cmp: Cmp,
        f: &mut impl FnMut(&'a Value, &'a Value),
    ) {
        // An in-order walk with an explicit stack, skipping the subtrees
        // that are wholly out of range.
        let mut stack: Vec<&'a Node> = Vec::new();
        let mut t = self.root.as_deref();
        loop {
            while let Some(n) = t {
                if lo.is_some_and(|lo| cmp(&n.key, lo) == Ordering::Less) {
                    // Everything to the left is smaller still.
                    t = n.right.as_deref();
                } else {
                    stack.push(n);
                    t = n.left.as_deref();
                }
            }
            let Some(n) = stack.pop() else { return };
            if hi.is_some_and(|hi| cmp(&n.key, hi) != Ordering::Less) {
                return;
            }
            f(&n.key, &n.value);
            t = n.right.as_deref();
        }
    }

    /// The bytes of the nodes no other tree shares (see `List::unshared_bytes`).
    pub fn unshared_bytes(&self) -> usize {
        fn bytes(t: &Link) -> usize {
            match t {
                Some(n) if Rc::strong_count(n) == 1 => {
                    size_of::<Node>() + bytes(&n.left) + bytes(&n.right)
                }
                _ => 0,
            }
        }
        bytes(&self.root)
    }

    /// Call `f` with the keys and values in this tree's nodes that aren't in
    /// `seen`, adding them to it, and return the bytes those nodes use (see
    /// `List::visit_unseen`).
    pub fn visit_unseen(&self, seen: &mut HashSet<usize>, mut f: impl FnMut(&Value)) -> usize {
        let mut bytes = 0;
        let mut work: Vec<&Rc<Node>> = self.root.iter().collect();
        while let Some(n) = work.pop() {
            if !seen.insert(Rc::as_ptr(n) as usize) {
                continue;
            }
            bytes += size_of::<Node>();
            f(&n.key);
            f(&n.value);
            work.extend(n.left.iter().chain(n.right.iter()));
        }
        bytes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cmp(a: &Value, b: &Value) -> Ordering {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            _ => panic!("not ints"),
        }
    }

    fn int(v: &Value) -> i64 {
        match v {
            Value::Int(i) => *i,
            _ => panic!("not an int"),
        }
    }

    fn keys(t: &Tree) -> Vec<i64> {
        let mut out = Vec::new();
        t.for_each(|k, _| out.push(int(k)));
        out
    }

    /// Check the sizes and the balance of every node, and the order of keys.
    fn check(t: &Tree) {
        fn walk(l: &Link) -> usize {
            let Some(n) = l else { return 0 };
            let (sl, sr) = (walk(&n.left), walk(&n.right));
            assert_eq!(n.size, sl + sr + 1);
            assert!(
                sl + sr <= 1 || (sl <= DELTA * sr && sr <= DELTA * sl),
                "{sl} / {sr}"
            );
            n.size
        }
        walk(&t.root);
        let ks = keys(t);
        assert!(ks.windows(2).all(|w| w[0] < w[1]), "out of order");
    }

    /// A shuffle of 0..n, from a linear congruential generator.
    fn shuffled(n: i64) -> Vec<i64> {
        let mut xs: Vec<i64> = (0..n).collect();
        let mut seed: u64 = 7;
        for i in (1..xs.len()).rev() {
            seed = seed
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            xs.swap(i, (seed >> 33) as usize % (i + 1));
        }
        xs
    }

    #[test]
    fn stays_balanced_and_in_order() {
        let mut t = Tree::default();
        // In order, which unbalanced trees handle worst, then shuffled.
        for k in (0..500).chain(shuffled(1000).into_iter().map(|k| k + 500)) {
            t.insert(Value::Int(k), Value::Int(k * 10), &cmp);
            if k % 50 == 0 {
                check(&t);
            }
        }
        check(&t);
        assert_eq!(t.len(), 1500);
        assert_eq!(keys(&t), (0..1500).collect::<Vec<_>>());
        assert_eq!(t.get(&Value::Int(777), &cmp).map(int), Some(7770));
        assert!(t.get(&Value::Int(1500), &cmp).is_none());
        assert_eq!(t.first().map(|(k, _)| int(k)), Some(0));
        assert_eq!(t.last().map(|(k, _)| int(k)), Some(1499));

        let before = t.clone();
        for k in shuffled(1500).into_iter().filter(|k| k % 3 != 0) {
            assert!(t.remove(&Value::Int(k), &cmp));
            if k % 50 == 0 {
                check(&t);
            }
        }
        assert!(!t.remove(&Value::Int(1), &cmp));
        check(&t);
        assert_eq!(
            keys(&t),
            (0..1500).filter(|k| k % 3 == 0).collect::<Vec<_>>()
        );
        assert_eq!(before.len(), 1500);
        check(&before);
        for k in 0..1500 {
            t.remove(&Value::Int(k), &cmp);
        }
        assert_eq!((t.len(), t.first().is_none()), (0, true));
    }

    #[test]
    fn ranges_are_half_open() {
        let mut t = Tree::default();
        for k in shuffled(100) {
            t.insert(Value::Int(k * 2), Value::Void, &cmp);
        }
        let range = |lo: Option<i64>, hi: Option<i64>| {
            let (lo, hi) = (lo.map(Value::Int), hi.map(Value::Int));
            let mut out = Vec::new();
            t.for_range(lo.as_ref(), hi.as_ref(), &cmp, &mut |k, _| out.push(int(k)));
            out
        };
        assert_eq!(range(Some(10), Some(20)), [10, 12, 14, 16, 18]);
        assert_eq!(range(Some(11), Some(19)), [12, 14, 16, 18]);
        assert_eq!(range(Some(195), None), [196, 198]);
        assert_eq!(range(None, Some(3)), [0, 2]);
        assert_eq!(range(Some(50), Some(50)), Vec::<i64>::new());
        assert_eq!(range(Some(60), Some(40)), Vec::<i64>::new());
        assert_eq!(range(None, None).len(), 100);
    }

    #[test]
    fn versions_count_only_what_they_dont_share() {
        let mut t = Tree::default();
        for k in shuffled(4000) {
            t.insert(Value::Int(k), Value::Int(k), &cmp);
        }
        let all = t.unshared_bytes();
        assert_eq!(all, 4000 * size_of::<Node>());
        let mut changed = t.clone();
        changed.insert(Value::Int(5), Value::Int(-5), &cmp);
        let path = changed.unshared_bytes();
        assert!(path <= 30 * size_of::<Node>(), "{path}");
        let mut seen = HashSet::new();
        assert_eq!(t.visit_unseen(&mut seen, |_| ()), all);
        assert_eq!(changed.visit_unseen(&mut seen, |_| ()), path);
    }
}
