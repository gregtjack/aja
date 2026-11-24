# Aja examples

Run any of these with `cargo run -- examples/<name>.aja`.

| Example | Shows |
|---------|-------|
| [hello.aja](hello.aja) | Values and types, `let`, `if` as an expression, `println` |
| [fizzbuzz.aja](fizzbuzz.aja) | `while` loops, a mutable `var`, `%`, nested `if` / `else`, string comparison |
| [recursion.aja](recursion.aja) | Recursion, mutual recursion, early `return`, tail calls in constant space, 100,000-deep calls |
| [closures.aja](closures.aja) | Functions as values, counters, `compose`, shared captured variables |
| [inference.aja](inference.aja) | Type inference inside function bodies, including closure parameters |
| [math.aja](math.aja) | Float math (Newton's method), primes, Collatz |
| [garbage.aja](garbage.aja) | The garbage collector freeing a million self-referencing closures |
| [strings.aja](strings.aja) | Template strings (`"{name} is {age}"`), a `to_str` method for a struct, string methods (`split`, `trim`, `join`, `parse_int`, ...), and the collector freeing big strings |
| [data.aja](data.aja) | Structs, enums with data, recursive types, and `match` with nested patterns and guards |
| [tuples.aja](tuples.aja) | Tuples: returning several values, `.0` / `.1`, patterns in `let`, `for`, and `match`, and comparing with `==` |
| [generics.aja](generics.aja) | Generic functions, structs, and enums: `Pair`, and a linked list with `map`, `fold`, and `find` |
| [lists.aja](lists.aja) | Lists: literals, indexing, `get`, `push` and `set` making new lists, `map` / `filter` / `fold`, and sorting |
| [collections.aja](collections.aja) | `std::collections`: counting words with a `Map` and `update`, ranking them with `sort_by` and `compare(...).then(...)`, `Set` operations, grouping into a `Map` of lists, a `SortedMap` of `SortedSet`s and its `range`, and maps comparing equal however they were built |
| [defer.aja](defer.aja) | `defer`: cleanup that runs on every way out of a block, `?` included, last first, at the end of each loop iteration, and seeing names as they were at the `defer` |
| [results.aja](results.aja) | `Option` and `Result` from the prelude, and passing errors on with `?` |
| [iterators.aja](iterators.aja) | `for` loops, ranges, `break` / `continue`, lazy `Iter` methods, an endless iterator, and a tree you can loop over |
| [methods.aja](methods.aja) | `impl` blocks: methods, associated functions, a generic stack, and the prelude's methods |
| [tasks.aja](tasks.aja) | `std::task`: spawning and joining tasks in a scope, a scope waiting for tasks nobody joins, a generic `par_map`, divide and conquer with nested scopes, errors as values from tasks, and two long loops taking turns |
| [channels.aja](channels.aja) | `std::channel`: a pipeline of tasks, a pool of workers sharing a queue, a rendezvous with no room, and shared state kept by one task that the others send requests |
| [shutdown.aja](shutdown.aja) | Cancelling a scope: a server whose accept loop and connections all stop on `cancel()`, each closing its connection in deferred code, while its clients, in another scope, carry on |
| [sleep.aja](sleep.aja) | Tasks running at the same time: three jobs that wait, done one after another and then at once, timed with `clock()` |
| [time.aja](time.aja) | `std::time`: timing a computation with `Instant`, making, adding, and comparing `Duration`s, and ticking at a steady rate with `sleep_until` |
| [net.aja](net.aja) | `std::net`: a server and three clients in one program, each connection in a task, `close_write` and reading to the end, and a refused connection |
| [net/echo_server.aja](net/echo_server.aja) | An echo server with a task per connection. Run `cargo run -- examples/net/echo_server.aja`, then `nc 127.0.0.1 7000` in another terminal |
| [fs/notes.aja](fs/notes.aja) | `std::fs`: appending to a file, checking it exists, its size, and reading its lines. Run `echo "buy milk" \| cargo run -- examples/fs/notes.aja` (it writes `notes.txt` in the working directory) |
| [io/greet.aja](io/greet.aja) | `std::io`: a prompt, reading lines until the input ends, standard error, and `main` returning a `Result`. Run `printf 'Ada\nfirst\nsecond\n' \| cargo run -- examples/io/greet.aja` |
| [modules/](modules) | A program in several files: `import`, `pub`, and a struct whose private field keeps an invariant. Run `cargo run -- examples/modules/main.aja` |

## Errors

The programs in [errors/](errors) are meant to fail, to show what Aja's
error messages look like. Type errors are reported before anything runs;
runtime errors point at the failing expression and list the calls that led
there.

| Example | Shows |
|---------|-------|
| [type_mismatch.aja](errors/type_mismatch.aja) | A mismatched type, with a note on int/float conversion |
| [missing_annotation.aja](errors/missing_annotation.aja) | A missing parameter type, with the annotation to add |
| [immutable.aja](errors/immutable.aja) | Assigning to a `let` and to a parameter, with how to make them mutable |
| [typo.aja](errors/typo.aja) | A misspelled name with a suggestion, and several errors reported at once |
| [call_trace.aja](errors/call_trace.aja) | Division by zero, with the call trace (recursion collapsed) |
| [overflow.aja](errors/overflow.aja) | Integer overflow, which is an error rather than a wraparound |
| [assert.aja](errors/assert.aja) | A failed `assert` |
| [task_failure.aja](errors/task_failure.aja) | An error inside a task, which stops the program |
| [cancelled_join.aja](errors/cancelled_join.aja) | `join` on a cancelled task, which has no value to give, and `try_join`, which gives `None` |
| [shared_var.aja](errors/shared_var.aja) | Spawning a task that uses a `var` of the spawning task, directly or through a function, and doing it with `let` copies and `join` instead |
| [deadlock.aja](errors/deadlock.aja) | Two tasks each waiting for the other's value, and the report of what every task waits for |
| [scope_ended.aja](errors/scope_ended.aja) | Spawning on a `Scope` kept after its `scope` call returned |
| [non_exhaustive.aja](errors/non_exhaustive.aja) | A `match` missing a case, an unreachable arm, and a name meant as a variant |
| [generic_body.aja](errors/generic_body.aja) | Generic code assuming things about a type parameter, and mismatched type arguments |
| [question_mark.aja](errors/question_mark.aja) | `?` in a function that doesn't return a `Result`, and with a different error type |
| [defer.aja](errors/defer.aja) | `return`, `?`, and `break` trying to leave deferred code |
| [loops.aja](errors/loops.aja) | Looping over something that isn't iterable, `break` outside a loop, and assigning to a loop variable |
| [methods.aja](errors/methods.aja) | A missing method, mixing up methods and associated functions, and an impl away from its type |

`cargo test` runs every example: the ones here must succeed, and each one in
`errors/` must fail with exactly the errors listed in its `// expect:` lines.
