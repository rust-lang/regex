Your friendly guide to hacking and navigating the regex library.

This guide assumes familiarity with Rust and Cargo, and at least a perusal of
the user facing documentation for this crate.

If you're looking for background on the implementation in this library, then
you can do no better than Russ Cox's article series on implementing regular
expressions using finite automata: https://swtch.com/~rsc/regexp/


## Architecture overview

As you probably already know, this library executes regular expressions using
finite automata. In particular, a design goal is to make searching linear with
respect to both the regular expression and the text being searched. Meeting
that design goal on its own is not so hard and can be done with an
implementation of the NFA algorithm, as described in:
https://swtch.com/~rsc/regexp/regexp2.html --- This library contains such an
implementation in src/nfa.rs. (N.B. PCRE's documentation also claims to
implement the NFA algorithm by citing Jeffrey Friedl's book, "Mastering Regular
Expressions." The book unfortunately conflates terminology.)

Making it fast is harder. One of the key problems with the NFA algorithm is
that it can be in more than one state at any point in time, and must shuffle
sub-capture positions between them. The NFA algorithm also spends a lot of
time following the same epsilon transitions over and over again. We can employ
one trick to speed up the NFA algorithm: extract one or more literal prefixes
from the regular expression and execute specialized code to quickly find
matches of those prefixes in the search text. The NFA algorithm can then be
avoided for most the search, and instead only executed when a prefix is found.
The code to find prefixes and search for prefixes is in src/literals.rs. When
more than one literal prefix is found, we fall back to an Aho-Corasick DFA
using the aho-corasick crate. For one literal, we use a variant of the
Boyer-Moore algorithm. Both Aho-Corasick and Boyer-Moore use `memchr` when
appropriate.

Of course, detecting prefix literals can only take us so far. Not all regular
expressions have literal prefixes. To remedy this, we try another approach to
executing the NFA: backtracking, whose implementation can be found in
src/backtrack.rs. One reason why backtracking can be faster is that it avoids
excessive shuffling of capture groups. Of course, backtracking is susceptible
to exponential runtimes, so we keep track of every state we've visited to make
sure we never visit it again. This guarantees linear time execution, but we
pay for it with the memory required to track visited states. Because of the
memory requirement, we only use this engine on small search strings *and* small
regular expressions.

Lastly, the real workhorse of this library is the "lazy" DFA in src/dfa.rs.
It is distinct from the NFA algorithm in that the DFA is explicitly represented
in memory and is only ever in one state at a time. It is said to be "lazy"
because the DFA is computed as text is searched, where each byte in the search
text results in at most one new DFA state. It is made fast by caching states.
DFAs are susceptible to exponential state blow up (where the worst case is
computing a new state for every input byte, regardless of what's in the state
cache). To avoid using a lot of memory, the lazy DFA uses a bounded cache. Once
the cache is full, it is wiped and state computation starts over again.

All of the above matching engines expose precisely the matching semantics. This
is indeed tested. (See the section below about testing.)

The following sub-sections describe the rest of the library and how each of the
matching engines are actually used.

### Parsing

Regular expressions are parsed using the regex-syntax crate, which is
maintained in this repository. The regex-syntax crate defines an abstract
syntax and provides very detailed error messages when a parse error is
encountered. Parsing is done in a separate crate so that others may benefit
from its existence, and because it is relatively divorced from the rest of the
regex library.

### Compilation

The compiler is in src/compile.rs. The input to the compiler is some abstract
syntax for a regular expression and the output is a sequence of opcodes that
matching engines use to execute a search. (One can think of matching engines as
mini virtual machines.) The sequence of opcodes is a particular encoding of a
non-deterministic finite automaton. In particular, the opcodes explicitly rely
on epsilon transitions.

Consider a simple regular expression like `a|b`. Its compiled form looks like
this:

    000 Save(0)
    001 Split(2, 3)
    002 'a' (goto: 4)
    003 'b'
    004 Save(1)
    005 Match

The first column is the instruction pointer and the second column is the
instruction. Save instructions indicate that the current position in the input
should be stored in a captured location. Split instructions represent a binary
branch in the program (i.e., epsilon transitions). The instructions `'a'` and
`'b'` indicate that the literal bytes `'a'` or `'b'` should match.

In older versions of this library, the compilation would looked like this:

    000 Save(0)
    001 Split(2, 3)
    002 'a'
    003 Jump(5)
    004 'b'
    005 Save(1)
    006 Match

In particular, empty instructions that merely served to move execution from one
point in the program to another were removed. Instead, every instruction has a
`goto` pointer embedded into it. This resulted in a small performance boost for
the NFA algorithm, because it was one fewer epsilon transition that it had to
follow.

There exist more instructions and they are defined and documented in
src/prog.rs.

Compilation has several knobs and a few unfortunately complicated invariants.
Namely, the output of compilation can be one of two types of programs: a
program that executes on Unicode scalar values or a program that executes on
raw bytes. In the former case, the matching engine is responsible for
performing UTF-8 decoding and executing instructions using Unicode codepoints.
In the latter case, the program handles UTF-8 decoding implicitly, so that the
matching engine can execute on raw bytes. All matching engines can execute
either Unicode or byte based programs except for the lazy DFA, which requires
byte based programs. In general, both representations were kept because (1) the
lazy DFA requires byte based programs so that states can be encoded in a memory
efficient manner and (2) the NFA algorithm benefits greatly from inlining
Unicode character classes into fewer instructions as it results in fewer
epsilon transitions. This means that every compiled program contains a proper
subset of all available instructions.

N.B. UTF-8 decoding is built into the compiled program by making use of the
utf8-ranges crate. The compiler in this library factors out common suffixes to
reduce the size of huge character classes (e.g., `\pL`).

A regrettable consequence of this split in instruction sets is we generally
need to compile two programs; one for NFA execution and one for the lazy DFA.

In fact, it is worse than that: the lazy DFA is not capable of finding the
starting location of a match in a single scan, and must instead execute a
backwards search after finding the end location. To execute a backwards search,
we must have compiled the regular expression *in reverse*.

This means that every compilation of a regular expression generally results in
three distinct programs. It would be possible to lazily compile the Unicode
program, since it is never needed if (1) the regular expression uses no word
boundary assertions and (2) the caller never asks for sub-capture locations.

### Execution

At the time of writing, there are four matching engines in this library:

1. The NFA algorithm (supports captures).
2. Bounded backtracking (supports captures).
3. Literal substring or multi-substring search.
4. Lazy DFA (no support for word boundary assertions).

Only the first two matching engines are capable of executing every regular
expression program. They also happen to be the slowest, which means we need
some logic that (1) knows various facts about the regular expression and (2)
knows what the caller wants. Using this information, we can determine which
engine (or engines) to use.

The logic for choosing which engine to execute is in src/exec.rs and is
documented on the Exec type. Exec values collection regular expression
Programs (defined in src/prog.rs), which contain all the necessary tidbits
for actually executing a regular expression on search text.

For the most part, the execution logic is straight-forward and follows the
limitations of each engine described above pretty faithfully. The hairiest part
of src/exec.rs by far is the execution of the lazy DFA, since it requires a
forwards and backwards search, and then falls back to either the NFA algorithm
or backtracking if the caller requested capture locations.

### Programs

A regular expression program is essentially a sequence of opcodes produced by
the compiler plus various caches for the matching engines plus various facts
about the regular expression (such as whether it is anchored, it capture names,
etc.).

The regular expression program contains mutable caches for use by the matching
engines. For example, the NFA algorithm stores its "execution threads" there,
the backtracking engine stores its visited map and the lazy DFA stores its
state cache. In sum, they are beneficial to performance because it allows reuse
of previously done work (especially for the lazy DFA). Mutation of these caches
should not be observable by callers, so it is done using interior mutability.
To make it possible to share regular expressions across threads, the cache is
guarded by a mutex but is not held during matching.

### The regex! macro (or why `regex::internal` exists)

The `regex!` macro is defined in the `regex_macros` crate as a compiled plugin,
which is maintained in this repository. The `regex!` macro compiles a regular
expression at compile time into specialized Rust code.

The `regex!` macro was written when this library was first conceived and
unfortunately hasn't changed much since then. In particular, it encodes the
entire NFA algorithm into stack allocated space (no heap allocation is done).
When `regex!` was first written, this provided a substantial speed boost over
so-called "dynamic" regexes compiled at runtime, and in particular had much
lower overhead per match. This was because the only matching engine at the time
was the NFA algorithm. The addition of other matching engines has inverted the
relationship; the `regex!` macro is almost never faster than the dynamic
variant. (In fact, it is typically substantially slower.)

In order to build the `regex!` macro this way, it must have access to some
internals of the regex library, which is in a distinct crate. (Compiler plugins
must be part of a distinct crate.) Namely, it must be able to compile a regular
expression and access its opcodes. The necessary internals are exported as part
of the top-level `internal` module in the regex library, but is hidden from
public documentation. In order to present a uniform API between programs build
by the `regex!` macro and their dynamic analoges, the `Regex` type is an enum
whose variants are hidden from public documentation.

In the future, the `regex!` macro should probably work more like Ragel, but
it's not clear how hard this is. In particular, the `regex!` macro should be
able to support all the features of dynamic regexes, which may be hard to do
with a Ragel-style implementation approach. (Which somewhat suggests that the
`regex!` macro may also need to grow conditional execution logic like the
dynamic variants, which seems rather grotesque.)


## Testing

A key aspect of any mature regex library is its test suite. A subset of the
tests in this library come from Glenn Fowler's AT&T test suite (its online
presence seems gone at the time of writing). The source of the test suite is
located in src/testdata. The scripts/regex-match-tests.py takes the test suite
in src/testdata and generates tests/matches.rs.

There are also many other manually crafted tests and regression tests in
tests/tests.rs.

The biggest source of complexity in the tests is related to answering this
question: how can we reuse the tests to check all of our matching engines?
One approach would have been to encode every test into some kind of format
(like the AT&T test suite) and code generate tests for each matching engine.
The approach we use in this library is to create a Cargo.toml entry point for
each matching engine we want to test. The entry points are:

* `tests/test_native.rs` - tests the `regex!` macro
* `tests/test_dynamic.rs` - tests `Regex::new`
* `tests/test_dynamic_nfa.rs` - tests `Regex::new`, forced to use the NFA
  algorithm on every regex.
* `tests/test_dynamic_nfa_bytes.rs` - tests `Regex::new`, forced to use the NFA
  algorithm on every regex and use byte based programs.
* `tests/test_dynamic_backtrack.rs` - tests `Regex::new`, forced to use
  backtracking on every regex.
* `tests/test_dynamic_backtrack_bytes.rs` - tests `Regex::new`, forced to use
  backtracking on every regex and use byte based programs.

The lazy DFA and pure literal engines are absent from this list because they
cannot be used on every regular expression. Instead, we rely on
`tests/test_dynamic.rs` to test the lazy DFA and literal engines when possible.

Since the tests are repeated several times, and because `cargo test` runs all
entry points, it can take a while to compile everything. To reduce compile
times slightly, try using `cargo test --test dynamic`, which will only use
the `tests/test_dynamic.rs` entry point.

N.B. To run tests for the `regex!` macro, use:

    cargo test --manifest-path regex_macros/Cargo.toml --test native


## Benchmarking

The benchmarking in this crate is made up of many micro-benchmarks. Currently,
there are two primary sets of benchmarks: the benchmarks that were adopted at
this library's inception (in `benches/bench.rs`) and a newer set of benchmarks
meant to test various optimizations. Specifically, the latter set contain some
analysis and are in `benches/bench_sherlock.rs`. Also, the latter set are all
executed on the same lengthy input whereas the former benchmarks are executed
on strings of varying length.

There is also a smattering of benchmarks for parsing and compilation.

Benchmarking follows a similarly wonky setup as tests. There are multiple
entry points:

* `bench_native.rs` - benchmarks the `regex!` macro
* `bench_dynamic.rs` - benchmarks `Regex::new`
* `bench_dynamic_nfa.rs` benchmarks `Regex::new`, forced to use the NFA
  algorithm on every regex. (N.B. This can take a few minutes to run.)
* `bench_pcre.rs` - benchmarks PCRE

The PCRE benchmarks exist as a comparison point to a mature regular expression
library. In general, this regex library compares favorably (there are even a
few benchmarks that PCRE simply runs too slowly on or outright can't execute at
all). I would love to add other regular expression library benchmarks
(especially RE2), but PCRE is the only one with reasonable bindings.

If you're hacking on one of the matching engines and just want to see
benchmarks, then all you need to run is:

    $ cargo bench --bench dynamic

If you want to compare your results with older benchmarks, then try:

    $ cargo bench --bench dynamic | tee old
    $ ... make it faster
    $ cargo bench --bench dynamic | tee new
    $ cargo-benchcmp old new --improvements

The `cargo-benchcmp` utility is available here:
https://github.com/BurntSushi/cargo-benchcmp

To run the same benchmarks on PCRE, you'll need to use the sub-crate in
`regex-pcre-benchmark` like so:

    $ cargo bench --manifest-path regex-pcre-benchmark/Cargo.toml

The PCRE benchmarks are separated from the main regex crate so that its
dependency doesn't break builds in environments without PCRE.
