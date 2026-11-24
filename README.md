
# Aja

Aja is a small, expression-oriented programming language written in Rust. The
goal is the language that is easiest for humans and agents to write correctly:
statically typed with inference, garbage collected, with green-thread
concurrency, running on a bytecode VM during development and compiled ahead
of time to native code through Cranelift. See [DESIGN.md](DESIGN.md) for the
principles, non-goals, and roadmap.

Today, Aja is **statically type-checked** with type inference, and runs on a
**register-based bytecode VM** with a REPL.

```rust
fn fact(n: int) -> int {
    if n == 0 { return 1; }
    n * fact(n - 1)
}

fn main() -> int {
    let start = clock();
    let add = fn(a: int, b: int) -> int { a + b };

    var i = 0;
    while i < 3 {
        print("i =", i, "\n");
        i = i + 1;
    }

    print("took", clock() - start, "ms\n");
    add(fact(5), 1)
}
```

## Getting started

Requires a Rust toolchain. A Nix flake (`flake.nix`, with `.envrc` for direnv)
provides a dev shell with Rust and `rust-analyzer`.

```bash
cargo build
```

Run a source file (executes `main`):

```bash
cargo run -- examples/hello.aja
```

[`examples/`](examples) has programs that show off the language, plus
some that fail on purpose to show the error messages.

Start the REPL (no arguments):

```bash
cargo run
```

The REPL accepts single-line statements or expressions; `help` lists
commands and `exit` / `quit` leaves.

Run the tests:

```bash
cargo test
```

## The language today

### Programs

A program is a sequence of top-level `fn`, `struct`, `enum`, and `impl`
definitions, in any order, after any `import`s (see [Modules](#modules)).
Execution starts at `fn main` in the file being run, which takes no
parameters. Its return value is not printed; use `println` for output.

### Types and values

| Type    | Literal examples        | Notes                                |
|---------|-------------------------|--------------------------------------|
| `int`   | `42`, `-7`, `1_000`     | 64-bit signed integer                |
| `float` | `3.14`, `-0.5`          | 64-bit float                         |
| `bool`  | `true`, `false`         |                                      |
| `str`   | `"hi\n"`, `"x is {x}"` | UTF-8; see [Strings](#strings). Escapes: `\n`, `\t`, `\r`, `\\`, `\"`, `\{`, `\}` |
| `void`  | `void`                  | No value: what statements and functions without `-> T` give; `Ok(void)` for a `Result<void, E>` |
| `(int, str) -> bool` | `fn(x: int) -> int { x + 1 }` | First-class; closures capture their scope |
| `List<T>` | `[1, 2, 3]`, `[]`        | Immutable; see [Lists](#lists)       |
| structs and enums | `Point { x: 1, y: 2 }`, `Shape::Circle(p, 3)` | Defined at the top level; see below |

### Strings

A string can hold expressions in braces. Each one is turned into a string
and put in its place, so there's no need to join pieces with `+`:

```rust
let name = "Ada";
println("{name} is {age + 1} next year");   // Ada is 37 next year
println("a literal \{brace}");               // a literal {brace}
```

- **What can go in a template:** a `str`, `int`, `float`, or `bool`, or a
  value of any type with a method `to_str(self) -> str`, which the template
  calls. Lists, functions, and values of a type parameter can't, since there
  is no single right way to show them; call a method that makes the string
  you want. Once there is a `Show` trait, `to_str` becomes it.
- Any expression works, including calls and other strings:
  `"{user.name.to_upper()} has {xs.len()} items"`.
- `\{` and `\}` write braces. A `}` with no `{` before it is just text.
- Templates can't be `match` patterns, since their value isn't known until
  the program runs.

Strings have methods (defined in the prelude):

| Method | Gives |
|--------|-------|
| `len()`, `byte_len()`, `is_empty()` | The number of characters, or of UTF-8 bytes |
| `contains(s)`, `starts_with(s)`, `ends_with(s)` | `bool` |
| `find(s)` | `Option<int>`: the position of the first `s` |
| `slice(start, end)` | The characters from `start` up to `end`; stops the program if out of range |
| `chars()`, `split(sep)`, `lines()`, `words()` | `List<str>` |
| `trim()`, `trim_start()`, `trim_end()`, `to_upper()`, `to_lower()` | A new string |
| `replace(from, to)`, `repeat(n)` | A new string |
| `sep.join(parts)` | The strings in `parts`, with `sep` between each two |
| `parse_int()`, `parse_float()` | `Option<int>`, `Option<float>` |
| `to_str()` | Itself |

**Positions count characters** (Unicode code points), not bytes:
`"héllo".len()` is 5 and `"héllo".slice(1, 2)` is `"é"`. Finding a
position takes time in proportion to it, so to go through a string a
character at a time, loop over `chars()`.

Numbers and bools have methods too: `n.to_str()`, `n.to_float()`, `n.abs()`,
`n.min(m)`, `n.max(m)`, `n.clamp(lo, hi)`, and `n.pow(e)` for `int`; `x.to_str()`,
`x.to_int()` (rounding towards zero), `floor`, `ceil`, `round`, `sqrt`,
`abs`, `min`, `max`, and `is_nan` for `float`; and `b.to_str()` for `bool`.
`3.min(4)` is a method call; `3.5` is a float.

### Structs, enums, and `match`

```rust
struct Point { x: int, y: int }

enum Shape {
    Circle(Point, int),
    Rect(Point, Point),
    Empty,
}

fn area(s: Shape) -> int {
    match s {
        Shape::Circle(_, r) => 3 * r * r,
        Shape::Rect(a, b) => (b.x - a.x) * (b.y - a.y),
        Shape::Empty => 0,
    }
}
```

- **Structs** have named fields: build one with `Point { x: 1, y: 2 }` (or
  `Point { x, y }` when variables have the fields' names) and read a field
  with `p.x`. Fields can't be assigned; make a new value instead, copying
  the fields you don't give from another with `Point { y: 5, ..p }`.
- **Enums** have variants that hold values by position, or none:
  `Shape::Circle(p, 3)`, `Shape::Empty`. Variants are written with their
  enum's name, except the prelude's `Some`, `None`, `Ok`, and `Err` (see
  [Option, Result, and `?`](#option-result-and-)). Types can be recursive (`enum List { Nil, Cons(int, List) }`)
  and can refer to each other in any order.
- **`match`** tries its arms in order: `pattern => value,` or
  `pattern if guard => value,`, where a value can be a block. Patterns are
  `_`, a name (which binds the value), int, bool, and string literals, variant
  patterns `Shape::Circle(center, r)`, and struct patterns
  `Point { x: 0, y }` (end with `..` to skip the other fields). They nest.
  **`a | b`** matches what either does: `4 | 6 | 9 | 11 => 30`, or
  `Shape::Circle(r) | Shape::Square(r) => r`, where each alternative must
  bind the same names to values of the same type. `|` works inside other
  patterns too: `Some(0 | 1)`.
  A `match` is an expression, and its arms are tail positions.
- **`match` is exhaustive.** Leaving a case out is a compile error that names
  the missing cases (`Shape::Empty` not covered), so adding a variant
  points you at every `match` to update. An arm that can never match, because
  earlier arms cover it, is an error too. Guarded arms don't count toward
  covering a case.
- **`==`** compares structs and enums field by field, as long as everything
  in them can be compared (no functions).
- In the head of an `if`, `while`, or `match`, a struct literal needs
  parentheses: `if p == (Point { x: 0, y: 0 }) { ... }`, as in Rust.

The checker for these lives in `src/check/data.rs`. In the VM, struct and
enum values are immutable heap objects: a tag, and the values they hold.

### Generics

Functions, structs, and enums can take type parameters, written in angle
brackets after the name:

```rust
struct Pair<A, B> { first: A, second: B }
enum List<T> { Nil, Cons(T, List<T>) }

fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> {
    Pair { first: p.second, second: p.first }
}

fn map<T, U>(xs: List<T>, f: (T) -> U) -> List<U> {
    match xs {
        List::Nil => List::Nil,
        List::Cons(x, rest) => List::Cons(f(x), map(rest, f)),
    }
}
```

- **Type arguments are inferred where code is used.** `Some(1)` is an
  `Option<int>`, `swap(Pair { first: 1, second: "a" })` is a
  `Pair<str, int>`, and each use of a generic function gets its own types,
  even when it's passed around as a value. When nothing decides one, as in
  `let x = None;` with `x` never used, the checker asks for an
  annotation (`x: Option<int>`). There is no syntax for writing type
  arguments at a call; an annotation does the same job.
- **Types written out always give their arguments:** `Option<int>`, never
  plain `Option`.
- **A generic body works for every type.** Inside `fn f<T>`, `T` is equal
  only to itself, so the body can pass values of type `T` around but can't
  add them, compare them, or read their fields, unless a bound says it can
  (see below). A type parameter must be used in its function's
  signature, or in its type's fields, since that's the only way to know what
  it is.
- **Bounds** ask more of a type parameter: `fn contains<T: Eq>(xs: List<T>,
  x: T) -> bool` can use `==` on its `T`s, and only gets called with types
  that support it. There are three traits, built in, and a type has them by
  its shape; programs can't define traits or implement them yet:

  | Trait  | Types that have it | Gives |
  |--------|--------------------|-------|
  | `Eq`   | int, float, bool, str, and structs, enums, tuples, and lists made of them | `==`, `!=` |
  | `Hash` | the same, without float (NaN isn't equal to itself) | `==`, `!=`; for map keys |
  | `Ord`  | int, bool, str, and structs, enums, tuples, and lists made of them | `==`, `!=`, `<`, `<=`, `>`, `>=`, `a.compare(b)`; for sorting |

  `Hash` and `Ord` each imply `Eq`. Combine bounds with `+`:
  `<K: Hash + Ord>`. Structs and enums can have bounds too
  (`struct Index<K: Hash> { ... }`), and every type written with them must
  meet them, so `fn f<K>(i: Index<K>)` is an error until it says `K: Hash`.
  An impl repeats its type's bounds (`impl<K: Hash> Index<K>`), and may ask
  for more, which every value its methods are called on must then meet.
  When a type falls short, the error says why: `P isn't Hash, because its
  field 'x' is float`.
- **What `Ord` means.** Values are ordered by their shape: strings by code
  point, `false` before `true`, a struct's fields and a tuple's parts in
  turn, an enum's variants in the order they are declared (then their
  values), and lists value by value, with a list before any longer list it
  starts. So `None < Some(0)`, `(1, "b") < (1, "c")`, and `[1, 2] < [1, 2, 0]`.
  `a.compare(b)` gives an `Ordering` (`Less`, `Equal`, or `Greater`, from
  the prelude) for any `Ord` values, unless their type has a `compare`
  method of its own; `o.reverse()` flips one, and `o.then(p)` breaks ties:
  `a.age.compare(b.age).then(a.name.compare(b.name))`.
- **`==` on a generic type** works when its contents can be compared:
  `Option<int>` can, `Option<(int) -> int>` can't.
- Generic code compiles once: every value has the same representation in the
  VM whatever its type, so nothing is copied per type.

### Option, Result, and `?`

Aja has no null and no exceptions. A value that may be missing is an
`Option`, and something that can fail returns a `Result`. Both are defined
in the prelude ([`src/prelude.aja`](src/prelude.aja)), which every program
can use without importing it:

```rust
enum Option<T> { None, Some(T) }   // None first, so None < Some(x)
enum Result<T, E> { Ok(T), Err(E) }
```

Their variants are written by themselves: `Some(1)`, `None`, `Ok(x)`,
`Err("bad input")`, in expressions and patterns. Writing `Option::Some(1)`
works too, since `Option` is an ordinary enum. Because the bare names always
mean the variants, they can't be used for variables, parameters, or
functions, and the prelude's types can't be redefined.

`expr?` takes the value out of an `Ok` or `Some`. On an `Err` or `None`, it
returns that from the enclosing function (or closure) right away:

```rust
fn sum(a: str, b: str) -> Result<int, str> {
    Ok(parse(a)? + parse(b)?)
}
```

- `?` on a `Result` needs the function to return a `Result` with the **same
  error type**, and `?` on an `Option` needs it to return an `Option`.
  Nothing is converted automatically: not one error type into another, and
  not a `None` into an error. Do it explicitly, with `match` or the
  prelude's methods: `parse(s).map_err(fn(e) { 404 })?`,
  `find(x).ok_or("not found")?`.
- `main` returns nothing, so it can't use `?`; it handles errors with
  `match`.
- A closure's return type is inferred from its `?`:
  `fn(s) { let n = parse(s)?; Ok(n * 2) }` returns a `Result<int, str>`.

The prelude gives both types methods:

| `Option<T>` | `Result<T, E>` |
|-------------|----------------|
| `is_some()`, `is_none()` | `is_ok()`, `is_err()` |
| `unwrap_or(default)` | `unwrap_or(default)` |
| `map(f)`, `and_then(f)` | `map(f)`, `and_then(f)`, `map_err(f)` |
| `or(other)`, `ok_or(error)` | `ok()` |

### Lists

```rust
let primes = [2, 3, 5, 7];
let more = primes.push(11).set(0, 1);   // [1, 3, 5, 7, 11]; primes is unchanged
println(primes[2], primes.get(10), primes.len());   // 5 None 4

var squares = [];
for i in 0..10 {
    squares = squares.push(i * i);
}
```

- **A list never changes.** `push`, `set`, `drop_last`, `concat`, and
  `slice` give a new list and leave the old one as it was, like a struct
  update. A `var` holding a list can be given the new one:
  `xs = xs.push(x)`. There is no `xs[i] = v`; write `xs = xs.set(i, v)`.
- **That's cheap.** A list is a persistent vector (as in Clojure): a tree
  with 32 values per node, which a new list shares almost all of. Reading,
  replacing, or adding a value takes a handful of steps whatever the
  length, so building a list a value at a time is fast.
- **`xs[i]`** is the value at index `i`, from 0. Out of range, it stops the
  program, like a division by zero: an index that might be out of range is
  what `xs.get(i)`, which returns an `Option`, is for.
- **Methods:** `len`, `is_empty`, `get`, `first`, `last`, `push`, `set`,
  `drop_last`, `concat`, `slice`, `reverse`, `map`, `filter`, `fold`,
  `find`, `any`, `all`, `iter`, `sort_by`, and `List::repeat(value, count)`.
  `iter.collect()` makes a list from an `Iter`.
- **Methods that need more of the values** (see [Bounds](#generics)):
  `contains`, `index_of`, and `dedup` (which drops values equal to the one
  before) for `Eq` values; `sort`, `min`, `max`, and `binary_search` for
  `Ord` values. A list of floats can't call `sort`, since NaN has no place
  in the order; `xs.sort_by(cmp)` sorts any list by a function returning an
  `Ordering`, such as `fn(a, b) { b.compare(a) }` for largest first. Both
  sorts are stable: equal values keep their order.
- `for x in xs` goes over a list's values (directly, without making an
  `Iter`). Lists compare with `==` value by value, and print as `[1, 2, 3]`.
- Every value in a list has the same type. An empty `[]` gets its type from
  how it is used, or from an annotation: `let xs: List<str> = [];`.

### Loops and iterators

```rust
for i in 0..10 {                 // 0, 1, ..., 9
    if i % 2 == 0 { continue; }
    if i > 7 { break; }
    println(i);
}

let squares = (1..11).map(fn(n) { n * n });
println(squares.filter(fn(n) { n % 2 == 0 }).fold(0, fn(a, b) { a + b }));
```

- **`for x in values { ... }`** runs its body once for each value. `values`
  is an **`Iter<T>`**, from the prelude, or a value whose type has a method
  `iter(self) -> Iter<T>`, which the loop calls: that is how a type of your
  own (or `Option`) can be looped over. `x` is immutable, and a new
  variable for each value, so closures made in the loop each keep their own.
- **`a..b`** is the `Iter<int>` from `a` up to, but not including, `b`
  (`Iter::range(a, b)`). `b` is evaluated once. A `for` loop over a range
  counts directly, without making an `Iter`.
- **An `Iter`** wraps a function that gives the next value, or `None` when
  there are no more: `Iter::new(fn() -> Option<T> { ... })`, keeping its
  place in a captured `var`. Its methods are `next`, `map`, `filter`,
  `take`, `skip`, `chain`, `fold`, `count`, `last`, `find`, `any`, and `all`,
  and `Iter::empty()` and `Iter::once(x)` make small ones. The methods that
  make a new `Iter` are lazy, so an endless `Iter` is fine as long as
  something stops it. An `Iter` is used up as it goes, and a copy of it
  shares its position.
- In the head of a `for`, as of an `if` or `while`, a struct literal needs
  parentheses: `for p in (Path { ... }) { ... }`.

The only traits are the built-in `Eq`, `Hash`, and `Ord` (see
[Generics](#generics)), so `Iter<T>` is a struct rather than a trait, and
"has a method `iter`" is checked by name. When programs can define traits,
the loop can take any type that implements an iterator trait, and these
programs keep working.

### `defer`

```rust
fn shout(conn: net::TcpStream) -> Result<void, io::Error> {
    defer conn.close();
    for line in conn.lines() {
        conn.write_line(line?.to_upper())?;   // `?` closes it too
    }
    Ok(void)
}
```

- **`defer expr;` or `defer { ... }`** puts code off until the block it is
  in is left, by reaching its end, `return`, `?`, `break`, or `continue`.
  Cleanup sits next to what it cleans up after, and runs on every way out.
- **Last first:** several in one block run in the reverse of the order
  they were written, after the block's value is worked out. Leaving several
  blocks at once (a `return` from inside a loop) runs the innermost block's
  first.
- **Block by block:** in a loop body, a `defer` runs at the end of each
  iteration, not of the function.
- **Names as at the `defer`:** the deferred code sees what was in scope
  where it is written, even if a later `let` shadows a name, and reads a
  `var` when it runs.
- **It can't leave by itself:** no `return` or `?`, and no `break` or
  `continue` out of it, since it runs while its block is already being left.
  A loop or closure inside it is fine.
- **Not on a runtime error:** a runtime error stops the whole program, and
  deferred code doesn't run then.
- A call returned while code is deferred isn't a tail call, since the
  deferred code runs after it (see [Tail calls](#architecture)).

### Methods

An `impl` block gives a struct or enum functions. One whose first parameter
is `self` is a **method**, called on a value with `.`; one without `self` is
an **associated function**, called through the type with `::`:

```rust
struct Vec2 { x: float, y: float }

impl Vec2 {
    fn new(x: float, y: float) -> Vec2 { Vec2 { x, y } }
    fn plus(self, other: Vec2) -> Vec2 { Vec2::new(self.x + other.x, self.y + other.y) }
    fn dot(self, other: Vec2) -> float { self.x * other.x + self.y * other.y }
}

let v = Vec2::new(1.0, 2.0).plus(Vec2::new(3.0, 4.0));
v.dot(v)
```

- **`self` has no annotation**: its type is the impl's type. Like any
  parameter, it can't be assigned to.
- **One way to call each.** A method is only called on a value
  (`v.dot(w)`, never `Vec2::dot(v, w)`), and an associated function only
  through its type. To pass a method around, wrap it in a closure:
  `fn(v) { v.dot(v) }`. An associated function is a value already:
  `let make = Vec2::new;`.
- **Generic types** declare their type parameters on the impl and give them
  as the type's arguments, `impl<T> Stack<T> { ... }`, for every `T` at
  once (there's no `impl Stack<int>`). Methods can add their own:
  `fn map<U>(self, f: (T) -> U) -> Stack<U>`.
- **A type's methods live next to it.** An `impl` must be in the same
  program as its type, so the prelude's types get their methods only in
  the prelude. A method can't share its name with one of its struct's
  fields or its enum's variants. A field that holds a function is still
  called as `s.f(x)`.
- The receiver's type must be known where a method is called, as for a
  field: in `fn(p) { p.norm() }`, annotate `p`.
- Built-in types (`int`, `float`, `bool`, `str`) get their methods from the
  prelude's `impl str { ... }` and so on (see [Strings](#strings)); a
  program can't add more.
- A method compiles to an ordinary function named like `Vec2::plus`, with
  `self` as its first argument, so tail calls and error traces work the
  same way.

### Modules

A file is a module, and the directory tree is the module tree: from the
directory of the file being run, `geo/shapes.aja` is the module
`geo::shapes`. There are no `mod` declarations.

```rust
// geo/shapes.aja
pub struct Point { pub x: int, pub y: int }
pub enum Shape { Circle(Point, int), Rect(Point, Point) }
pub fn area(s: Shape) -> int { ... }
fn helper() -> int { ... }      // private: only this file can use it

// main.aja
import geo::shapes;             // shapes::area, shapes::Shape::Circle, ...
import geo::shapes::{Point};    // Point by itself
import geo::shapes::area;       // so is a single item without braces

fn main() {
    let s = shapes::Shape::Circle(Point { x: 0, y: 0 }, 2);
    println(area(s));
}
```

- **`import a::b;`** makes module `a::b`'s items available through its last
  name, `b::item`; **`import a::b::{X, y};`** brings in those names by
  themselves, and **`import a::b::X;`** brings in just `X`. A path
  without braces names a module if there is one (`a/b/X.aja`), and
  otherwise an item of the module before it. There are no
  glob imports, so every name in a file is defined there, in the prelude, or
  in an import. Imports come before any definition.
- **Private by default.** Functions, structs, enums, methods, and struct
  fields are visible only inside their file unless marked `pub`. A struct
  with private fields can't be built outside its module (provide a function
  that makes one), and a pattern for it there must end in `..`. An enum's
  variants are as visible as the enum. The standard library is the one
  exception: it is written as one package, so its modules can use each
  other's private items (programs still can't).
- **No import cycles.** A cycle is an error that names every module in it.
  The file being run can't be imported (that would always be a cycle).
- **Methods live with their type**: an `impl` must be in the module that
  defines its type.
- The REPL has no imports yet.

[examples/modules](examples/modules) is a program in several files.

### Type checking

Every program is type-checked before it runs, and every type error is
reported (all of them, not just the first) without running anything. There
is no `any` type: every value has one static type.

- **Signatures are written out.** Top-level functions annotate every
  parameter. A function with no `-> T` returns `void`. Function types are
  written without `fn`, as the parameter types in parentheses and the
  return type, which is always given: `(int, str) -> bool`, or
  `(int) -> void` for one that returns nothing. Leaving out `fn` keeps a
  type from looking like a closure. Higher-order functions are declared
  with them:
  `fn apply(f: (int) -> int, x: int) -> int { f(x) }`.
- **Everything inside a body is inferred.** `let` bindings, closure
  parameters, and closure return types need no annotations; their types come
  from how they're used:

  ```rust
  let twice = fn(f, x) { f(f(x)) };       // f: (int) -> int, x: int
  twice(fn(y) { y * 2 }, 3)
  ```

- **One type per binding.** A closure has a single type, fixed by its uses,
  so `let id = fn(x) { x }; id(1); id(true);` is an error. A function
  that works at many types is declared generic in its signature instead:
  `fn id<T>(x: T) -> T { x }` (see [Generics](#generics)).
- If a type can't be worked out (e.g. a closure parameter that's never
  used), the checker asks for an annotation rather than guessing.

The checker lives in `src/check/`. It is unification-based, the core of
Hindley–Milner inference, without let-polymorphism. Operators that accept
several types (`+` on int, float, or str; `==` on int, float, bool, or str) keep
an unresolved type restricted until it's known, much like Rust's `{integer}`.

```
error: mismatched types: function 'one' has no return type, so it returns void, but this is int
 --> main.aja:1:12
  |
1 | fn one() { 1 }
  |            ^
  = help: add a return type after the parameters: `-> int`
```

### Syntax

- **Variables:** `let name: type = expr;` declares an immutable variable;
  `var name: type = expr;` declares a mutable one, which `name = expr`
  assigns. Assigning to a `let`, a parameter, or a name bound by a pattern is
  a type error; to change a parameter, copy it first (`var n = n;`). Blocks
  introduce a new lexical scope, and a later `let` or `var` with the same
  name shadows the earlier one.
- **Functions:** `fn name(a: int, b: str) -> int { ... }`. Parameters must be
  annotated; omitting `-> T` means the function returns `void`.
- **Closures:** an anonymous `fn` with the same syntax as a named function,
  minus the name: `fn(a, b) { a + b }`, `fn(x: int) -> int { x * 2 }`,
  `fn() { ... }`. The body is always a block, and annotations are optional.
  Closures capture their
  enclosing environment and can mutate its `var`s (e.g. counters). A closure
  bound with `let` or `var` can call itself by that name:
  `let fact = fn(n: int) { if n == 0 { 1 } else { n * fact(n - 1) } };`.
  Other values don't see the name they are bound to, so `let a = a + 1;`
  uses an earlier `a`.
- **Blocks are expressions:** the final expression without a trailing `;` is
  the block's value. `return expr;` exits a function early.
- **`if`:** `if cond { ... } else { ... }` is an expression; `if` without
  `else` is a statement. Chain conditions with `else if`:
  `if n < 0 { "negative" } else if n == 0 { "zero" } else { "positive" }`.
  An `if` used as a value needs a final `else`.
- **`while`:** `while cond { ... }`.
- **`for`:** `for x in values { ... }` (see [Loops and iterators](#loops-and-iterators)).
  `break` leaves the innermost `while` or `for` loop, and `continue` starts
  its next iteration.
- **`defer`:** `defer expr;` or `defer { ... }` runs code when its block is
  left (see [`defer`](#defer)).
- **Operators** (lowest to highest precedence):
  `=` → `..` → `||` → `&&` → `==` `!=` → `<` `<=` `>` `>=` → `+` `-` → `*` `/` `%`
  → unary `!` `-` → calls `f(x)(y)`, field access `p.x`, indexing `xs[i]`,
  and `?`. As in C and Rust, `a == b && c < d` means
  `(a == b) && (c < d)`. `&&` and `||` short-circuit. Integer division or modulo by zero is a
  runtime error.
  Arithmetic requires both operands to be the same numeric type (no
  implicit int/float conversion). `<`, `<=`, `>`, and `>=` work on two ints,
  two floats, or two values of any `Ord` type, such as strings (see
  [Generics](#generics)).
- **Comments:** `// to end of line`.

### Built-in functions

| Function         | Description                                      |
|------------------|--------------------------------------------------|
| `print(a, b, …)` | Prints its arguments separated by spaces (no trailing newline) |
| `println(a, b, …)` | Like `print`, followed by a newline            |
| `clock()`        | Current time in milliseconds, as an `int`        |
| `sleep(ms)`      | Sleeps for `ms` milliseconds                     |
| `assert(cond)`   | Stops execution with an error if `cond` is false |

To add a built-in, declare its name and signature in `BUILTINS`
(`src/builtins.rs`), which the type checker reads, and implement it in each
backend (for the VM, `src/vm/native.rs`). A built-in without an
implementation fails the tests and stops the VM at startup. Built-ins named
`std::native::...` are the standard library's intrinsics (see below).

### The standard library

The standard library is written in Aja (`src/stdlib/`), built into the
binary, and imported like any module, by paths starting with `std`. So far
it has `std::collections`, `std::io`, `std::fs`, `std::net`, `std::task`, and `std::time`.

#### `std::collections`

```rust
import std::collections::{Map, Set};

fn main() {
    var counts = Map::new();
    for w in "a b a c a".words() {
        counts = counts.update(w, fn(n) { Some(n.unwrap_or(0) + 1) });
    }
    println(counts.get("a"));              // Some(3)
    for (word, n) in counts { println("{word}: {n}"); }

    let seen = Set::from([3, 1, 3]);
    println(seen.contains(1), seen.len());  // true 2
}
```

| `Map<K: Hash, V>` | Does |
|-------------------|------|
| `Map::new()`, `Map::from(pairs)` | An empty map, or one of a list of `(key, value)` pairs (a repeated key gets its last value) |
| `len()`, `is_empty()`, `contains_key(k)` | |
| `get(k)` | `Option<V>` |
| `insert(k, v)`, `remove(k)` | A new map, with `k` given `v`, or without `k` |
| `update(k, f)` | A new map where `f(get(k))` decides: `Some(v)` stores `v`, `None` removes `k` |
| `keys()`, `values()`, `to_list()` | `List<K>`, `List<V>`, `List<(K, V)>`, in the map's order |
| `iter()` | `Iter<(K, V)>`, so `for (k, v) in m` works |
| `map_values(f)`, `filter(f)`, `merge(other)` | A new map; `merge` takes `other`'s value for a key both have |

| `Set<T: Hash>` | Does |
|----------------|------|
| `Set::new()`, `Set::from(values)` | |
| `len()`, `is_empty()`, `contains(v)` | |
| `insert(v)`, `remove(v)` | A new set |
| `union(other)`, `intersection(other)`, `difference(other)`, `is_subset(other)` | |
| `to_list()`, `iter()` | The values, in the set's order |

`SortedMap<K: Ord, V>` and `SortedSet<T: Ord>` have the same methods, keep
their keys in order (so they go over them, and print them, from smallest to
largest), and add:

| `SortedMap` / `SortedSet` | Does |
|---------------------------|------|
| `first()`, `last()` | The entry (or value) with the smallest or largest key, as an `Option` |
| `range(start, end)` | The entries (or values) with keys from `start` up to, but not including, `end`, in order, as a `List` |

```rust
let by_time = SortedMap::from([(30, "c"), (10, "a"), (20, "b")]);
println(by_time.first());                       // Some((10, "a"))
for (t, e) in by_time.range(15, 35) { ... }     // 20 b, then 30 c
```

- **Maps and sets never change**, like lists: `m = m.insert(k, v)` on a
  `var`. Each is a hash array mapped trie (`src/vm/hamt.rs`), so a new
  version shares all but a handful of nodes with the old one, and
  inserting, removing, and looking up take a few steps whatever the size.
- **Keys must be `Hash`** (see [Bounds](#generics)): ints, strings, bools,
  and structs, enums, tuples, lists, maps, and sets made of them, but not
  floats or functions. `Map<float, int>` is a compile error that says why.
- **There is no `m[k]`**: a missing key is an ordinary thing, not a bug, so
  `get` returns an `Option`.
- **Order:** entries come in an order decided by their keys' hashes, the
  same on every run, but not one to rely on; `m.keys().sort()` when order
  matters. Maps and sets print as `{"a": 1, "b": 2}` and `{1, 2}`.
- **`==` compares contents**: maps with the same keys and values are equal
  however they were built, and hash the same, so a `Set<Set<int>>` holds
  each set once. Maps and sets aren't `Ord`; sorted maps and sets are,
  entry by entry in key order, as lists of pairs would be.
- **Use `Map` and `Set` unless order matters.** The sorted ones are
  weight-balanced binary trees (`src/vm/sorted.rs`), whose steps take
  about log₂ n comparisons, and need `Ord` keys rather than `Hash` ones.

#### `std::io`


```rust
import std::io;

fn main() -> Result<void, io::Error> {
    let out = io::stdout();
    out.write("What's your name? ")?;
    match io::stdin().read_line()? {
        Some(name) => out.write_line("Hello, " + name + "!"),
        None => out.write_line("Goodbye!"),
    }
}
```

| `std::io`                          | Does |
|------------------------------------|------|
| `stdin()`, `stdout()`, `stderr()`  | A `Reader` or `Writer` for the standard streams |
| `reader.read_line()`               | `Result<Option<str>, Error>`: the next line without its `\n` or `\r\n`, or `None` at the end of input |
| `reader.lines()`                   | `Iter<Result<str, Error>>`: each line left, for a `for` loop; a failed read is the last value |
| `reader.read_to_string()`          | `Result<str, Error>`: everything left |
| `writer.write(text)`, `writer.write_line(text)` | `Result<void, Error>` |
| `writer.flush()`                   | Write out buffered output now |
| `Error { kind, message }`, `ErrorKind` | Why an operation failed: `NotFound`, `PermissionDenied`, `InvalidData`, `BrokenPipe`, `AlreadyExists`, `NotADirectory`, `IsADirectory`, `DirectoryNotEmpty`, or `Other` |

- Everything that can fail returns a `Result`. **`main` can return a
  `Result`**, so `?` works there: if it returns an `Err`, the program prints
  `error: main returned an error: ...` and exits with a failure status.
- `stdout()` shares its buffer with `print` and `println`, so output comes
  out in order. Reading standard input flushes standard output first, so a
  prompt written without a newline shows before the program waits.
- I/O blocks. There will be no separate asynchronous API; concurrency is to
  come from green threads.

#### `std::fs`

```rust
import std::fs;
import std::io;

fn main() -> Result<void, io::Error> {
    fs::write("notes.txt", "one\ntwo\n")?;
    for line in fs::read_lines("notes.txt")? {
        println(line);
    }
    fs::remove_file("notes.txt")
}
```

| `std::fs`                                  | Does |
|--------------------------------------------|------|
| `read_to_string(path)`                     | `Result<str, io::Error>`: the whole file, which must be UTF-8 |
| `read_lines(path)`                         | `Result<List<str>, io::Error>`: its lines, without `\n` or `\r\n` |
| `write(path, text)`, `append(path, text)`  | Replace or add to a file's text, creating it if needed |
| `copy(from, to)`, `rename(from, to)`       | Copy a file; move a file or directory |
| `remove_file(path)`                        | Delete a file |
| `read_dir(path)`                           | `Result<List<str>, io::Error>`: the names in a directory, sorted |
| `create_dir(path)`, `create_dir_all(path)` | Make a directory (and, with `_all`, its missing parents) |
| `remove_dir(path)`, `remove_dir_all(path)` | Delete an empty directory, or one and everything in it |
| `exists(path)`, `is_file(path)`, `is_dir(path)` | `bool`, never failing |
| `size(path)`                               | `Result<int, io::Error>`: a file's size in bytes |

- Paths are `str`s, relative to the working directory. Errors are
  `io::Error`s whose message names the path: `could not read 'notes.txt':
  no such file or directory`.
- Each function opens and closes the file itself, so there are no file
  handles to close. Reading a file a piece at a time can come with
  handles once there's a way to be sure they get closed.
- Underneath, the library calls a few **intrinsics** in the module
  `std::native` (`read_line`, `write`, `flush`, ...), in
  `src/stdlib/native.aja`. Only the standard library can import it.
  Intrinsics take and return only `int`, `float`, `bool`, `str`, `void`,
  and lists of those, so a backend implements them without knowing Aja's
  types; one
  that can fail records a status, which the library turns into a `Result`.
- **Native code is declared in Aja.** The prelude and standard library can
  declare `native fn len(self) -> int;` (a function with no body, which each
  backend implements, in `src/vm/native.rs` for the VM, under the name it
  is compiled as, such as `List::len`) and `native struct List<T>;` (a type
  whose values only native functions make). A test checks that the VM
  implements every one. Programs can't declare either.
- To add a module, write `src/stdlib/<name>.aja` and list it in `MODULES`
  in `src/stdlib/mod.rs`.

#### `std::net`

```rust
import std::io;
import std::net;
import std::task;

fn echo(conn: net::TcpStream) -> Result<void, io::Error> {
    defer conn.close();
    for line in conn.lines() {
        conn.write_line(line?)?;
    }
    Ok(void)
}

fn main() -> Result<void, io::Error> {
    let listener = net::listen("127.0.0.1:7000")?;
    task::scope(fn(s) {
        while true {
            match listener.accept() {
                Ok(conn) => { s.spawn(fn() { echo(conn) }); }
                Err(e) => println(e.message),
            }
        }
    });
    Ok(void)
}
```

| `std::net` | Does |
|------------|------|
| `listen(addr)` | `Result<TcpListener, io::Error>`; port 0 picks a free port |
| `connect(addr)` | `Result<TcpStream, io::Error>`, looking up a name first if there is one |
| `listener.accept()` | `Result<TcpStream, io::Error>`, waiting for the next connection |
| `stream.read_line()`, `stream.lines()`, `stream.read_to_string()` | As on `io::Reader`; the end is when the other end finishes sending |
| `stream.write(text)`, `stream.write_line(text)` | `Result<void, io::Error>`, returning once all of it is sent |
| `stream.close_write()` | Tell the other end nothing more is coming, while still reading |
| `local_addr()`, `stream.peer_addr()`, `close()` | On both; addresses are `str`s like `"127.0.0.1:7000"` |

- **Each operation blocks only its task.** Sockets are non-blocking
  underneath: one that isn't ready parks its task with a poller (epoll,
  kqueue, or IOCP, through the `polling` crate), and the other tasks run.
  So a server gives each connection a task, written as plain code.
  Connecting runs on a thread of its own, since it can't be done without
  blocking.
- **Streams carry text**, which must be UTF-8. Reads are buffered, so a
  line or character that arrives in pieces comes out whole. Bytes wait for
  a bytes type.
- Errors are `io::Error`s naming the address, with the kinds
  `ConnectionRefused`, `ConnectionReset`, `AddrInUse`, `AddrNotAvailable`,
  and `NotConnected` (a closed socket). Closing a socket wakes any task
  waiting on it, with `NotConnected`.
- Sockets stay open until closed, or until the program ends. There are no
  timeouts yet. See [plans/net.md](plans/net.md) for what comes next.

#### `std::time`

```rust
import std::time::{Duration, Instant};
import std::time;

fn main() {
    let start = Instant::now();
    time::sleep_for(Duration::from_millis(250));
    println("took {start.elapsed()}");    // took 250.1ms
}
```

| `std::time` | Does |
|-------------|------|
| `Duration::from_nanos(n)`, `from_micros`, `from_millis`, `from_secs`, `from_secs_float(1.5)`, `zero()` | A span of time |
| `d.as_nanos()`, `as_micros()`, `as_millis()`, `as_secs()` | `int`, rounded towards zero; `as_secs_float()` gives a `float` |
| `d.add(other)`, `sub(other)`, `mul(n)`, `div(n)`, `abs()` | A new `Duration` |
| `d.is_zero()`, `d.is_negative()` | |
| `Instant::now()` | The moment it is now, on a clock that only goes forward |
| `i.elapsed()`, `i.since(earlier)` | The `Duration` from then until now, or from `earlier` until `i` |
| `i.add(d)`, `i.sub(d)` | The `Instant` `d` later or earlier |
| `sleep_for(d)`, `sleep_until(instant)` | Pause the calling task (only it), while other tasks run |

- A `Duration` is a whole number of nanoseconds in an `int`, so it reaches
  about 292 years, and can be negative (`a.since(b)` when `a` came first).
  Sleeping for a negative duration, or until a moment that has passed,
  returns straight away.
- Durations and instants compare with `<`, `==`, and the rest. Durations
  show in the largest unit they have one of, with up to three decimals:
  `1.5s`, `250ms`, `12.345µs`, `800ns`.
- An `Instant` means nothing on its own, only next to another from the same
  run. There is no calendar time yet; `clock()` still gives milliseconds
  since 1970.
- `sleep_for` isn't `sleep` because the built-in `sleep(ms)` has that name.

## Architecture

```
files ──► Loader ──► Lexer ──► Parser ──► Resolver ──► AST ──► Type checker ──► Compiler ──► Bytecode VM
          module/    lexer.rs  parse.rs   module/      ast/    check/           vm/compile.rs  vm/mod.rs
```

| Path                  | Role |
|-----------------------|------|
| `src/main.rs`         | CLI (clap): file mode and REPL |
| `src/token.rs`        | Token definitions |
| `src/lexer.rs`        | Hand-written lexer; tracks line/column for errors |
| `src/parse.rs`        | Recursive-descent parser (plus unit tests) |
| `src/ast/`            | AST nodes (`Definition`, `Statement`, `Expression`) and `Type` |
| `src/module/`         | Modules: the loader reads the file being run and every module it imports, rejecting cycles; `resolve.rs` gives each definition a module-qualified name (`geo::shapes::area`), rewrites every use to it, and checks that items used across modules are `pub`. The result is one program for the checker and backends |
| `src/check/`          | Static type checker with local type inference; `data.rs` checks structs, enums, and `match` exhaustiveness, and `methods.rs` checks `impl` blocks and method calls |
| `src/builtins.rs`     | Built-in function signatures, shared by the checker and backends, including the standard library's intrinsics (`std::native`) |
| `src/stdlib/`         | The standard library, written in Aja and built into the binary (`io.aja` is `std::io`, `fs.aja` is `std::fs`) |
| `src/prelude.aja`, `src/prelude.rs` | The prelude (`Option`, `Result`, and their methods), written in Aja and built into the binary. Locations in it are marked as the prelude's, so errors in code it calls point at the right file |
| `src/vm/bytecode.rs`  | The instruction set and compiled functions (`Proto`) |
| `src/vm/compile.rs`   | Compiles the type-checked AST to bytecode |
| `src/vm/mod.rs`       | The VM: call frames, the dispatch loop, runtime errors |
| `src/vm/value.rs`     | Runtime values |
| `src/vm/heap.rs`      | Garbage-collected heap for closures, cells, strings, struct and enum values, and lists |
| `src/vm/scheduler.rs` | Tasks: the run queue, switching, timers, and waiting on sockets |
| `src/vm/net.rs`       | TCP for `std::net`: the socket table and the poller |
| `src/vm/list.rs`      | Lists: persistent vectors that share structure between versions |
| `src/vm/hamt.rs`      | Maps and sets: hash array mapped tries that share structure between versions |
| `src/vm/sorted.rs`    | Sorted maps and sets: weight-balanced trees that share structure between versions |
| `src/vm/native.rs`    | Built-in implementations |
| `src/diagnostic.rs`   | Error rendering (source snippets, carets, help, color), and the source files each location can point into |

The VM is register-based. Each call gets a window of registers holding its
parameters, locals, and temporaries, and instructions name registers
directly. Instructions are typed: the type checker records whether each
operator works on ints, floats, bools, or strings, and the compiler emits,
say, `AddI` or `AddF` accordingly, so the VM never inspects operand types to
decide what to do and has no runtime type checks.

Calls are guaranteed to be tail calls when they're in tail position: the
final expression of a function (including both branches of a final
`if`/`else`) or the value of a `return`, unless code deferred with `defer`
is waiting to run after it. A tail call reuses the current
frame, so tail recursion runs in constant space, like a loop. Other calls
keep their frame until they return. Frames are kept in a growable array, not
on the Rust stack, so deep recursion can't crash the VM. The call stack may
use up to 256 MB (about 1.5 million simple non-tail calls); beyond that the
program stops with a stack overflow error. Error traces list a tail call
like any other call, without mentioning tail calls; the caller it replaced
is no longer on the stack, so it isn't listed.

A closure copies the variables it captures when it is made, which is all an
immutable variable needs. A `var` that a closure captures is stored in a
shared cell instead, and closures copy the cell, so the function and its
closures see each other's assignments. The compiler finds these variables in
a pass over each function before compiling it. A closure that calls itself
through its `let` captures itself, filled in as soon as it is made.

Closures, cells, struct and enum values, and strings live on a garbage-collected heap (`src/vm/heap.rs`). The
collector is a precise mark-and-sweep: it starts from the registers in use,
the globals, and the call frames, marks every object reachable from them, and
frees the rest, so cycles (such as a closure that calls itself through a
variable it captures) are freed too. It runs when the VM allocates, once as
many bytes have been allocated since the last collection as survived it, so
a few large strings trigger it as surely as many small objects. A new version
of a list counts only the nodes it doesn't share with the one it came from,
and the collector visits (and counts) a node that versions share once, so
building a list a value at a time stays linear. String literals are put
on the heap once, when the code using them is loaded, and kept as roots;
strings built with `+` are freed once unreachable. A string may be at most
`i32::MAX` bytes; building a longer one is a runtime error. The VM tests run with the
collector in stress mode, collecting before every allocation, so any value
the VM forgets to keep reachable is caught.

## Status

**Working:** ints, floats, bools, strings; immutable `let` and mutable
`var` with lexical scoping; functions and first-class closures; `if`/`else` expressions;
`while` loops; `return`; `defer`; recursion, with guaranteed tail calls; arithmetic, comparison, and logical operators;
`for` loops over iterators, ranges, `break` and `continue`; immutable lists
with indexing;
structs, enums with data, and exhaustive `match`; generic functions, structs, and enums,
with `Eq`, `Hash`, and `Ord` bounds; ordering with `<` and `compare` on any `Ord` type; `Option`,
`Result`, and `Ordering` in the prelude, and `?`; methods and associated functions in `impl` blocks;
sorting and searching lists;
modules, with `import` and `pub`; the start of the standard library
(`std::collections`, `std::io`, `std::fs`, `std::net`, `std::task`, `std::time`); template strings, and methods on strings and
numbers; static type checking with inference inside function bodies; built-ins (`print`, `clock`, `sleep`,
`assert`); a REPL; parser, type checker, and VM unit tests.

**Reserved or partially present, not yet usable:**
- `<-` is lexed but has no grammar.
- There is no native code backend yet; one using Cranelift is planned (see
  [DESIGN.md](DESIGN.md)).

**Errors:** errors are reported in a rustc-like format and exit with a
non-zero status. Syntax and parse errors point at the offending source:

```
error: expected ';', found 'x'
 --> main.aja:2:12
  |
2 |   let x = 1
  |            ^
  = help: add ';' at the end of the statement
```

Type errors (see [Type checking](#type-checking)) point at the offending
expression, say what was expected and what was found, and suggest close
matches for misspelled names.

Runtime errors, such as division by zero or a failed `assert`, point at the
expression that failed and list the calls that led there (repeated
recursive calls are collapsed):

```
error: division by zero
 --> main.aja:1:23
  |
1 | fn c(x: int) -> int { 100 / x }
  |                       ^^^^^^^
  = note: in function 'c', called at line 2, column 23
  = note: in function 'b', called at line 3, column 20
```

Every AST expression carries a `Span` (`ast::Expr { kind, span }`), and the
compiler records the span each instruction came from, which is how runtime
errors know their location. In the REPL, each input counts as a
new line of the session, so errors in code from an earlier input still point
at that input.

Output is colored when stderr is a terminal; set `NO_COLOR` to disable.
`src/diagnostic.rs` holds the shared renderer.

**Known limitations:**
- The REPL handles one line at a time and can't define top-level functions, structs, or enums.
- There is no `else if`; nest an `if` inside the `else` block instead.
- Only `//` line comments are supported.
- `==` and `!=` bind more loosely than `&&` and `||`, so write
  `(a == b) && (c == d)`.
