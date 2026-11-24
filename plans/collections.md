# Plan: collections

The `collections` row of the library's Core layer (see DESIGN.md, "The
standard library"): maps, sets, and ordered versions of both, which
`std::net`, `json`, `http`, and most real programs need before anything
else. It also settles prerequisite 3 of that section, the core traits,
because a hash map can't be written without them.

## The problem

A `Map<K, V>` has to hash and compare its keys. Today a type parameter has
no bounds: inside `fn f<T>`, `T` is equal only to itself, so a generic body
can't use `==` on it, and nothing can hash or order it. The checker already
knows *which concrete types* can be compared (`Class::Eq` in
`src/check/mod.rs`, structural over structs, enums, and tuples); it just has
no way for a signature to ask for it.

## Decisions

| Question | Decision |
|---|---|
| Bounds | **Three built-in traits, `Eq`, `Hash`, and `Ord`, usable as bounds: `fn f<K: Hash, V>`.** No user-defined traits and no `impl Trait for T` yet. A type has a trait by its shape, as it has `==` today. This answers the open question "do traits start as a fixed built-in set?" with "yes, for now", and leaves room for user traits. |
| Which types | **`Eq`:** what `==` accepts today. **`Hash`:** `Eq` without `float` (NaN ≠ NaN would make a key unfindable). **`Ord`:** `int`, `bool`, `str`, and structs, enums, tuples, and lists of `Ord` types; no `float`, no functions. A `float` key or sort is written explicitly (`sort_by`, or a key of `x.to_bits()`). |
| What `Ord` means | **Structural, lexicographic.** Tuples and structs by field order, enums by variant order then fields, lists element by element, strings by code point, `false < true`. `Option` is reordered to `None, Some(T)` so that `None` sorts first, as in Rust. |
| Bounds on types | **Declared on the type, repeated on impls and functions.** `native struct Map<K: Hash, V>;` makes `Map<float, int>` an error wherever it's written or inferred, and `fn f<K, V>(m: Map<K, V>)` is an error whose help adds `K: Hash`. Nothing is implied, so a signature says everything (rule 5). |
| `<` and friends | **On any `Ord` type**, including `str` and a `T: Ord`, not just numbers. `float` keeps its IEEE `<` as today. |
| Mutability | **Persistent, like `List`.** `m.insert(k, v)` returns a new map and leaves `m` alone; `m = m.insert(k, v)` on a `var`. Values stay immutable, and tasks can share maps freely. |
| Implementation | **Native, in Rust, like `List`.** A HAMT for `Map`/`Set`, a persistent balanced tree for `SortedMap`/`SortedSet`, both with `Rc` nodes changed through `Rc::make_mut` as in `src/vm/list.rs`. Native gives `==`, hashing, and printing that don't depend on the tree's shape, which an Aja implementation would get wrong (two equal sorted maps built in different orders have different trees, and structural `==` would call them different). The Cranelift backend links the same Rust runtime, so this isn't written twice. |
| Iteration order | **`Map`/`Set`: unspecified but deterministic** (same program and input, same order), from SipHash-1-3 with a fixed key. **`SortedMap`/`SortedSet`: by key.** Seeding the hash per process, against hash flooding, is revisited with `std::net`. |
| Indexing | **No `m[k]`.** A missing key is an ordinary event, not a bug, so lookups return `Option`: `m.get(k)`. (Lists keep `xs[i]`, where a bad index usually is a bug.) |
| Literals | **No new syntax.** `Map::from([("a", 1), ("b", 2)])`, `Set::from([1, 2, 3])`. |
| Where | **`std::collections`, imported by name**: `import std::collections::{Map, Set};`. Not the prelude, which stays small. |
| "One map" | **`Map` is the map.** `SortedMap` exists for a different job (order and ranges), and its docs say to use `Map` otherwise. |

## The surface

```aja
import std::collections::{Map, Set, SortedMap};

fn word_counts(text: str) -> Map<str, int> {
    var counts = Map::new();
    for w in text.words() {
        counts = counts.update(w, fn(n) { Some(n.unwrap_or(0) + 1) });
    }
    counts
}

fn main() {
    let counts = word_counts("a b a c a");
    println(counts.get("a"));             // Some(3)
    println(counts);                      // {"a": 3, "b": 1, "c": 1}  (some order)
    for (word, n) in counts { println("{word}: {n}"); }

    let seen = Set::from([3, 1, 3]);
    println(seen.contains(1), seen.len()); // true 2

    let by_time = SortedMap::from([(30, "c"), (10, "a"), (20, "b")]);
    println(by_time.first());             // Some((10, "a"))
    for (t, e) in by_time.range(15, 35) { println("{t} {e}"); }
}
```

`Map<K: Hash, V>`:

| Method | Gives |
|---|---|
| `Map::new()`, `Map::from(pairs)` | A map; later pairs win |
| `len()`, `is_empty()` | |
| `get(k)`, `contains_key(k)` | `Option<V>`, `bool` |
| `insert(k, v)`, `remove(k)` | A new map |
| `update(k, f)` | `f(get(k))` decides: `Some(v)` stores `v`, `None` removes |
| `keys()`, `values()`, `iter()` | `Iter<K>`, `Iter<V>`, `Iter<(K, V)>`; `for (k, v) in m` |
| `map_values(f)`, `filter(f)`, `merge(other)` | A new map; `merge` prefers `other` |
| `to_list()` | `List<(K, V)>` |

`Set<T: Hash>`: `new`, `from`, `len`, `is_empty`, `contains`, `insert`,
`remove`, `union`, `intersection`, `difference`, `is_subset`, `iter`,
`to_list`.

`SortedMap<K: Ord, V>` and `SortedSet<T: Ord>`: the same methods, plus
`first`, `last`, `range(lo, hi)` (from `lo` up to, not including, `hi`, as
`..` is), and iteration in order.

And on `List`, once bounds exist: `contains` and `index_of` (`T: Eq`),
`sort`, `min`, `max`, `binary_search` (`T: Ord`), `sort_by(cmp)` for any
`T`, and `dedup` (`T: Eq`). An `Ordering` enum (`Less`, `Equal`,
`Greater`) joins the prelude, with `a.compare(b)` on any `Ord` type.

All four collections compare with `==` by contents, are `Hash` when their
contents are (so a `Set<Set<int>>` works), print like `{"a": 1}` and
`{1, 2}`, and can go in templates only through `to_str`, as lists can't
today, until there's a `Show` trait.

## Runtime design

- **Hashing and ordering in the VM.** `Heap::hash(&Value, &mut SipHasher)`
  and `Heap::compare(&Value, &Value) -> Ordering`, next to the existing
  `Heap::equal`, walking strings, data, lists, and the new collections
  structurally. The checker guarantees they're never given a float,
  function, task, or scope, so they can treat one as unreachable.
- **`Value::Map(Gc)`, `Value::Set(Gc)`, and the sorted pair**, each a heap
  object holding the Rust structure. The collector traces their keys and
  values, as it does a list's.
- **HAMT** (`src/vm/hamt.rs`): 32-way, bitmap-indexed nodes, collision
  buckets kept sorted by `compare` where possible and by insertion
  otherwise. Removal collapses nodes, so equal maps have equal shapes and
  `==` can compare node by node; length plus a lookup per entry is the
  fallback.
- **Weight-balanced tree** (`src/vm/sorted.rs`, planned as a B-tree; see
  step 5): binary nodes that know their size, rebuilt along the path to
  the key on each change.
- **Natives never call back into Aja** (the scheduler depends on this), so
  `update`, `map_values`, and `filter`, which take closures, are written in
  Aja in `collections.aja` over native `get`/`insert`/`remove`/`iter`.

## Steps

Each is a commit with tests, and the language works after each.

1. ✅ **Bounds in the checker.** Parse `<T: Eq + Hash>` on functions, structs,
   enums, and impls. A bounded parameter admits the matching operators in
   the body. Check bounds at every use: instantiation, written types, and
   passing a bounded generic on. Generalize `Class` from a chain into sets
   of traits. Diagnostics name the missing bound and suggest adding it.
   Done as planned; also, an impl may ask for more than its type's bounds
   (`impl<T: Ord> Wrapper<T>`), and calling its methods then requires it,
   which is how step 3 can give `List<T>` a `sort` for `T: Ord` only. `<`
   on a `T: Ord` waits for step 2. `Ord` doesn't imply `Hash`.
2. ✅ **`Ord` operators and `Ordering`.** `<` on `Ord` types, `compare`, the
   `Option` reordering, structural compare in the VM. Done: `a.compare(b)`
   is the one method every `Ord` type has without defining it (a type's
   own `compare` comes first), and calls the prelude's private
   `Ordering::of`; `Ordering` has `reverse` and `then`. Floats keep `<`
   but have no `compare`.
3. ✅ **`List` methods** that need bounds: `contains`, `sort`, `min`, `max`,
   `dedup`, `binary_search`, `sort_by`. A quick win that tests steps 1–2
   end to end. Done: `sort` is native (Rust's stable sort over
   `Heap::compare`), the rest Aja, in `impl<T: Eq> List<T>` and
   `impl<T: Ord> List<T>`; `sort_by` is a bottom-up merge sort in Aja,
   since natives can't call closures, and `binary_search` returns
   `Option<int>`. It found a collector bug: a list version was counted at
   its full length, so collections came every few pushes and each marked
   every node of every version. Now a version counts its unshared nodes,
   and marking visits shared nodes once (100,000-value `sort_by`: 53 s to
   0.5 s).
4. ✅ **Hashing in the VM, and `Map` and `Set`** (`std::collections`).
   Done: `src/vm/hamt.rs` is the trie for both (a set's values are
   `void`), counted and marked like lists' nodes; `Heap::hash` uses the
   standard library's SipHash with fixed keys, and a map's hash is a sum
   over its entries so it doesn't depend on their order. Natives return
   only lists and primitives, so `get` is `contains_key` and a private
   `value`, and `keys` and `values` return `List`s (not `Iter`s, as first
   planned: a list has `len`, `sort`, and the rest), with `iter` and
   `to_list` pairing them up in Aja. Also changed from step 1: an impl's
   bounds are checked at a method call only where they ask more than the
   type's own, which are checked where the type is written. 100,000
   string-keyed inserts take 0.2 s in a release build.
5. ✅ **`SortedMap` and `SortedSet`.** Done, with weight-balanced binary
   trees (`src/vm/sorted.rs`, Adams' trees with ∆ = 3 and Γ = 2, as in
   Haskell's `Data.Map`) rather than the planned B-tree: they are
   persistent by construction, deletion is a few lines, and each node
   knows its size, which leaves room for indexing by rank later. They are
   `Eq`, `Hash`, and `Ord`, compared entry by entry in key order; `range`
   returns a `List` like `keys` does.
6. ✅ **Docs and examples**: `examples/collections.aja`, README, DESIGN.md
   roadmap.

Later, not in this plan: `Deque` (an RRB vector or finger tree, for cheap
pushes at the front), a priority queue, a `Show` trait so templates take
lists and maps, and user-defined traits.
