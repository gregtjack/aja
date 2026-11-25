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
- [x] Bytes, or a character from a code point, so `%C3%A9` can be decoded:
      `s.bytes()`, `str::from_utf8(bytes)`, `n.to_char()`
- [x] Reading text that isn't UTF-8 ends the connection; no way to answer 400
      (it already took the bad bytes off the stream; the server now answers)
- [x] Command-line arguments and environment variables: `std::env`
- [x] Socket timeouts: `set_timeout` on listeners and streams (not connect)
- [x] A string builder, or a warning in the docs: `s = s + x` in a loop is
      quadratic (20k appends: 411 ms, against 44 ms for push + join).
      Documented; a real builder or a faster `+` is still to do

## Found since

- [x] No hex literals: `0x1F600` doesn't lex
- [ ] `var` is a keyword, so `std::env` has `get` where Rust has `var`
- [ ] `return`/`break`/`continue` still can't be operands outside a match
      arm (`x.unwrap_or(return 0)`)
- [ ] Connecting has no timeout

## Tooling

- [x] Tests only run top-level examples, so `examples/net/*.aja` is never
      even type-checked

## Since `std::http`

The server's parsing moved into [`std::http`](../src/stdlib/http.aja),
with a client. It needed:

- [x] `stream.read_bytes(n)`, since a chunk of a chunked body can end
      partway through a character
- [x] `io::ErrorKind::InvalidInput`, for a URL the client can't use

Still to do:

- [ ] TLS, for `https://`
- [ ] A bytes type, so bodies needn't be UTF-8 text (images, gzip)
- [ ] Streaming bodies, in both directions, rather than whole strings
- [ ] Connection pooling in the client, and following redirects
- [ ] A timeout on connecting, which the client needs too
- [ ] Calendar time, for a `Date` header
- [ ] Closing a server gracefully: today it cancels the requests in flight
