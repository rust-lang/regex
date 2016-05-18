0.1.70
======
* [PR #231](https://github.com/rust-lang-nursery/regex/pull/231):
  Add SIMD accelerated multiple pattern search.
* [PR #228](https://github.com/rust-lang-nursery/regex/pull/228):
  Reintroduce the reverse suffix literal optimization.
* [PR #226](https://github.com/rust-lang-nursery/regex/pull/226):
  Implements NFA state compression in the lazy DFA.
* [PR #223](https://github.com/rust-lang-nursery/regex/pull/223):
  A fully anchored RegexSet can now short-circuit.

0.1.69
======
* [PR #216](https://github.com/rust-lang-nursery/regex/pull/216):
  Tweak the threshold for running backtracking.
* [PR #217](https://github.com/rust-lang-nursery/regex/pull/217):
  Add upper limit (from the DFA) to capture search (for the NFA).
* [PR #218](https://github.com/rust-lang-nursery/regex/pull/218):
  Add rure, a C API.

0.1.68
======
* [PR #210](https://github.com/rust-lang-nursery/regex/pull/210):
  Fixed a performance bug in `bytes::Regex::replace` where `extend` was used
  instead of `extend_from_slice`.
* [PR #211](https://github.com/rust-lang-nursery/regex/pull/211):
  Fixed a bug in the handling of word boundaries in the DFA.
* [PR #213](https://github.com/rust-lang-nursery/regex/pull/213):
  Added RE2 and Tcl to the benchmark harness. Also added a CLI utility from
  running regexes using any of the following regex engines: PCRE1, PCRE2,
  Oniguruma, RE2, Tcl and of course Rust's own regexes.

0.1.67
======
* [PR #201](https://github.com/rust-lang-nursery/regex/pull/201):
  Fix undefined behavior in the `regex!` compiler plugin macro.
* [PR #205](https://github.com/rust-lang-nursery/regex/pull/205):
  More improvements to DFA performance. Competitive with RE2. See PR for
  benchmarks.
* [PR #209](https://github.com/rust-lang-nursery/regex/pull/209):
  Release 0.1.66 was semver incompatible since it required a newer version
  of Rust than previous releases. This PR fixes that. (And `0.1.66` was
  yanked.)

0.1.66
======
* Speculative support for Unicode word boundaries was added to the DFA. This
  should remove the last common case that disqualified use of the DFA.
* An optimization that scanned for suffix literals and then matched the regular
  expression in reverse was removed because it had worst case quadratic time
  complexity. It was replaced with a more limited optimization where, given any
  regex of the form `re$`, it will be matched in reverse from the end of the
  haystack.
* [PR #202](https://github.com/rust-lang-nursery/regex/pull/202):
  The inner loop of the DFA was heavily optimized to improve cache locality
  and reduce the overall number of instructions run on each iteration. This
  represents the first use of `unsafe` in `regex` (to elide bounds checks).
* [PR #200](https://github.com/rust-lang-nursery/regex/pull/200):
  Use of the `mempool` crate (which used thread local storage) was replaced
  with a faster version of a similar API in @Amanieu's `thread_local` crate.
  It should reduce contention when using a regex from multiple threads
  simultaneously.
* PCRE2 JIT benchmarks were added. A benchmark comparison can be found
  [here](https://gist.github.com/anonymous/14683c01993e91689f7206a18675901b).
  (Includes a comparison with PCRE1's JIT and Oniguruma.)
* A bug where word boundaries weren't being matched correctly in the DFA was
  fixed. This only affected use of `bytes::Regex`.
* [#160](https://github.com/rust-lang-nursery/regex/issues/160):
  `Captures` now has a `Debug` impl.
