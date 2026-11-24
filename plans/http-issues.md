# Issues found writing an HTTP server

From writing [examples/net/http_server.aja](../examples/net/http_server.aja).

## Language

- [x] `return`, `break`, `continue` as a match arm's value, so
      `None => return Err(e),` works (not yet as operands elsewhere,
      which would need them to be expressions of type never)
- [x] The struct-literal hint fires wrongly: `None => break,` after
      `while x < len {` suggests `(len { ... })`
- [x] `if` as an operand: `a + if c { x } else { y }` (`match` already was)

## Standard library

- [x] `stream.read(n)`: read exactly `n` bytes, for `Content-Length`
      bodies (today only lines or everything to the end)
- [ ] Bytes, or a character from a code point, so `%C3%A9` can be decoded
- [ ] Reading text that isn't UTF-8 ends the connection; no way to answer 400
- [ ] Command-line arguments and environment variables
- [ ] Socket timeouts
- [ ] A string builder, or a warning in the docs: `s = s + x` in a loop is
      quadratic (20k appends: 411 ms, against 44 ms for push + join)

## Tooling

- [x] Tests only run top-level examples, so `examples/net/*.aja` is never
      even type-checked
