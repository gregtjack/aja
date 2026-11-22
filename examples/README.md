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
| [strings.aja](strings.aja) | Joining strings with `+`, and the collector freeing big strings |
| [data.aja](data.aja) | Structs, enums with data, recursive types, and `match` with nested patterns and guards |

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
| [call_trace.aja](errors/call_trace.aja) | Division by zero, with the call trace (recursion collapsed, tail calls marked) |
| [overflow.aja](errors/overflow.aja) | Integer overflow, which is an error rather than a wraparound |
| [assert.aja](errors/assert.aja) | A failed `assert` |
| [non_exhaustive.aja](errors/non_exhaustive.aja) | A `match` missing a case, an unreachable arm, and a name meant as a variant |

`cargo test` runs every example: the ones here must succeed, and each one in
`errors/` must fail with exactly the errors listed in its `// expect:` lines.
