0.1.80
======
* [PR #292](https://github.com/rust-lang-nursery/regex/pull/292):
  Fixes bug #291, which was introduced by PR #290.

0.1.79
======
* Require regex-syntax 0.3.8.

0.1.78
======
* [PR #290](https://github.com/rust-lang-nursery/regex/pull/290):
  Fixes bug #289, which caused some regexes with a certain combination
  of literals to match incorrectly.

0.1.77
======
* [PR #281](https://github.com/rust-lang-nursery/regex/pull/281):
  Fixes bug #280 by disabling all literal optimizations when a pattern
  is partially anchored.

0.1.76
======
* Tweak criteria for using the Teddy literal matcher.

0.1.75
======
* [PR #275](https://github.com/rust-lang-nursery/regex/pull/275):
  Improves match verification performance in the Teddy SIMD searcher.
* [PR #278](https://github.com/rust-lang-nursery/regex/pull/278):
  Replaces slow substring loop in the Teddy SIMD searcher with Aho-Corasick.
* Implemented DoubleEndedIterator on regex set match iterators.

0.1.74
======
* Release regex-syntax 0.3.5 with a minor bug fix.
* Fix bug #272.
* Fix bug #277.
* [PR #270](https://github.com/rust-lang-nursery/regex/pull/270):
  Fixes bugs #264, #268 and an unreported where the DFA cache size could be
  drastically under estimated in some cases (leading to high unexpected memory
  usage).

0.1.73
======
* Release `regex-syntax 0.3.4`.
* Bump `regex-syntax` dependency version for `regex` to `0.3.4`.

0.1.72
======
* [PR #262](https://github.com/rust-lang-nursery/regex/pull/262):
  Fixes a number of small bugs caught by fuzz testing (AFL).

0.1.71
======
* [PR #236](https://github.com/rust-lang-nursery/regex/pull/236):
  Fix a bug in how suffix literals were extracted, which could lead
  to invalid match behavior in some cases.

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
