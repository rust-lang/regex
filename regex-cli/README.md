regex-cli
=========
This is a command line tool for interacting with the regex, regex-automata and
regex-syntax crates. It enables one to print debug representations of various
values, run searches, generate DFAs and deserialization code and perform
various regex development tasks such as generating tests.

### Installation

Currently `regex-cli` is not on crates.io and should be installed from this
git repository:

```
$ cargo install --git https://github.com/rust-lang/regex regex-cli
```


### Example: print debug output

The `regex-cli` command provides a way to print the debug output for most
of the principle types in the `regex-automata` crate. This can be useful for
debugging purposes when working on the `regex` project, or even if you just
want a better look at a regex object's internal representation. For example,
the following two commands compare and contrast the differences in the NFA for
`.` and `(?-u:.)`:

```
$ regex-cli debug thompson '.' --no-table

thompson::NFA(
>000000: binary-union(2, 1)
 000001: \x00-\xFF => 0
^000002: capture(pid=0, group=0, slot=0) => 10
 000003: \x80-\xBF => 11
 000004: \xA0-\xBF => 3
 000005: \x80-\xBF => 3
 000006: \x80-\x9F => 3
 000007: \x90-\xBF => 5
 000008: \x80-\xBF => 5
 000009: \x80-\x8F => 5
 000010: sparse(\x00-\t => 11, \x0B-\x7F => 11, \xC2-\xDF => 3, \xE0 => 4, \xE1-\xEC => 5, \xED => 6, \xEE-\xEF => 5, \xF0 => 7, \xF1-\xF3 => 8, \xF4 => 9)
 000011: capture(pid=0, group=0, slot=1) => 12
 000012: MATCH(0)

transition equivalence classes: ByteClasses(0 => [\x00-\t], 1 => [\n], 2 => [\x0B-\x7F], 3 => [\x80-\x8F], 4 => [\x90-\x9F], 5 => [\xA0-\xBF], 6 => [\xC0-\xC1], 7 => [\xC2-\xDF], 8 => [\xE0], 9 => [\xE1-\xEC], 10 => [\xED], 11 => [\xEE-\xEF], 12 => [\xF0], 13 => [\xF1-\xF3], 14 => [\xF4], 15 => [\xF5-\xFF], 16 => [EOI])
)
```

And now for `(?-u:.)`:

```
$ regex-cli debug thompson -b '(?-u:.)' --no-table

thompson::NFA(
>000000: binary-union(2, 1)
 000001: \x00-\xFF => 0
^000002: capture(pid=0, group=0, slot=0) => 3
 000003: sparse(\x00-\t => 4, \x0B-\xFF => 4)
 000004: capture(pid=0, group=0, slot=1) => 5
 000005: MATCH(0)

transition equivalence classes: ByteClasses(0 => [\x00-\t], 1 => [\n], 2 => [\x0B-\xFF], 3 => [EOI])
)
```

To make things a bit more concise, we use `--no-table` to omit some extra
metadata about the size of the NFA and the time required to build it.

In the second example, we also pass the `-b/--no-utf8-syntax` flag. Without
it, the command returns an error because patterns are compiled with default
settings. The default setting is to forbid any pattern that can possibly match
invalid UTF-8. Since `(?-u:.)` matches any byte except for `\n`, it can match
invalid UTF-8. Thus, you have to say, "I am explicitly okay with matching
invalid UTF-8."


### Example: execute a search

This command shows how to run a search with multiple patterns with each
containing capture groups. The output shows the value of each matching group.

```
$ regex-cli find capture meta -p '(?m)^(?<key>[[:word:]]+)="(?<val>[^"]+)"$' -p $'(?m)^(?<key>[[:word:]]+)=\'(?<val>[^\']+)\'$' -y 'best_album="Blow Your Face Out"'
     parse time:  81.541µs
 translate time:  52.035µs
build meta time:  805.696µs
    search time:  426.391µs
  total matches:  1
0:{ 0: 0..31/best_album="Blow\x20Your\x20Face\x20Out", 1/key: 0..10/best_album, 2/val: 12..30/Blow\x20Your\x20Face\x20Out }
```

In this case, `meta` refers to the regex engine. It can be a number of other
things, including `lite` for testing the `regex-lite` crate. Also, `capture`
refers to the kind of search. You can also just ask for the `match` which will
print the overall match and not the capture groups:

```
$ regex-cli find match meta -p '(?m)^(?<key>[[:word:]]+)="(?<val>[^"]+)"$' -p $'(?m)^(?<key>[[:word:]]+)=\'(?<val>[^\']+)\'$' -y 'best_album="Blow Your Face Out"'
     parse time:  67.067µs
 translate time:  40.005µs
build meta time:  586.163µs
    search time:  291.633µs
  total matches:  1
0:0:31:best_album="Blow\x20Your\x20Face\x20Out"
```

Since not all regex engines support capture groups, using `match` will open up
the ability to test other regex engines such as `hybrid`.

Finally, the `-p/--pattern` flag specifies a pattern and the `-y/--haystack`
flag provides a haystack to search as a command line argument. One can also omit
the `-y/--haystack` flag and provide a file path to search instead:

```
$ echo 'best_album="Blow Your Face Out"' > haystack
$ regex-cli find match hybrid -p '(?m)^(?<key>[[:word:]]+)="(?<val>[^"]+)"$' -p $'(?m)^(?<key>[[:word:]]+)=\'(?<val>[^\']+)\'$' haystack
               parse time:  60.278µs
           translate time:  43.832µs
 compile forward nfa time:  462.148µs
 compile reverse nfa time:  56.275µs
build forward hybrid time:  6.532µs
build reverse hybrid time:  4.089µs
         build regex time:  4.899µs
      cache creation time:  18.59µs
              search time:  54.653µs
            total matches:  1
0:0:31:best_album="Blow\x20Your\x20Face\x20Out"
```


### Example: serialize a DFA

One particularly useful command in `regex-cli` is `regex-cli generate
serialize`. It takes care of generating and writing a fully compiled DFA to
a file, and then producing Rust code that deserializes it. The command line
provides oodles of options, including all options found in the `regex-automata`
crate for building the DFA in code.

Let's walk through a complete end-to-end example. We assume `regex-cli` is
already installed per instructions above. Let's start with an empty binary
Rust project:

```
$ mkdir regex-dfa-test
$ cd regex-dfa-test
$ cargo init --bin
```

Now add a dependency on `regex-automata`. Technically, the only feature that
needs to be enabled for this example is `dfa-search`, but we include `std` as
well to get some conveniences like `std::error::Error` implementations and also
optimizations. But you can drop `std` and just use `alloc` or even drop `alloc`
too altogether if necessary.

```
$ cargo add regex-automata --features std,dfa-search
```

Now we can generate a DFA with `regex-cli`. This will create three files: the
little endian binary serialization of the DFA, the big endian version and a
simple Rust source file for lazily deserializing the DFA via a static into a
`regex_automata::util::lazy::Lazy`:

```
regex-cli generate serialize sparse dfa \
  --minimize \
  --shrink \
  --start-kind anchored \
  --rustfmt \
  --safe \
  SIMPLE_WORD_FWD \
  ./src/ \
  "\w"
```

We pass a number of flags here. There are even more available, and generally
speaking, there is at least one flag for each configuration knob available in
the library. This means that it should be possible to configure the DFA in any
way you might expect to be able to in the code. We can briefly explain the
flags we use here though:

* `--minimize` applies a [DFA minimization] algorithm to try and shrink
the size of the DFA as much as possible. In some cases it can make a big
difference, but not all. Minimization can also be extremely expensive, but
given that this is an offline process and presumably done rarely, it's usually
a good trade off to make.
* `--shrink` uses heuristics to make the size of the NFA smaller in some cases.
This doesn't impact the size of the DFA, but it can make determinization (the
process of converting an NFA into a DFA) faster at the cost of making NFA
construction slower. This can make overall DFA generation time faster.
* `--start-kind anchored` says to build a DFA that only supports anchored
searches. (That is, every match must have a start offset equivalent to the
start of the search.) Without this, DFAs support both anchored and unanchored
searches, and that in turn can make them much bigger than they need to be if
you only need one or the other.
* `--rustfmt` will run `rustfmt` on the generated Rust code.
* `--safe` will use only safe code for deserializing the DFA. This may be
slower, but it is a one time cost. If you find that deserializing the DFA is
too slow, then dropping this option will use alternative APIs that may result
in undefined behavior if the given DFA is not valid. (Every DFA generated by
`regex-cli` is intended to be valid. So *not* using `--safe` should always be
correct, but it's up to you whether it's worth doing.)

[DFA minimization]: https://en.wikipedia.org/wiki/DFA_minimization

The final three positional arguments are as follows:

* `SIMPLE_WORD_FWD` is the name of the variable in the Rust source code for
the DFA, and it is also used in generating the names of the files produced by
this command.
* `./src/` is the directory to write the files.
* `\w` is the regex pattern to build the DFA for. More than one may be given!

Once the DFA is generated, you should see three new files in `./src/`:

```
$ ls -l src/
total 32
-rw-rw-r-- 1 andrew users    45 May 28 22:04 main.rs
-rw-rw-r-- 1 andrew users 11095 May 30 10:24 simple_word_fwd.bigendian.dfa
-rw-rw-r-- 1 andrew users 11095 May 30 10:24 simple_word_fwd.littleendian.dfa
-rw-rw-r-- 1 andrew users   711 May 30 10:24 simple_word_fwd.rs
```

At this point, you just need to add the appropriate `mod` definition in
`main.rs` and use the DFA:

```rust
use regex_automata::{dfa::Automaton, Anchored, Input};

use crate::simple_word_fwd::SIMPLE_WORD_FWD as DFA;

mod simple_word_fwd;

fn main() {
    let input = Input::new("ω").anchored(Anchored::Yes);
    println!("is a word: {:?}", DFA.try_search_fwd(&input));

    let input = Input::new("☃").anchored(Anchored::Yes);
    println!("not a word: {:?}", DFA.try_search_fwd(&input));
}
```

And now run the program:

```
$ cargo run
   Compiling regex-dfa-test v0.1.0 (/home/andrew/tmp/regex-dfa-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.17s
     Running `target/debug/regex-dfa-test`
is a word: Ok(Some(HalfMatch { pattern: PatternID(0), offset: 2 }))
not a word: Ok(None)
```

There are a few other things worth mentioning:

* The above generates a "sparse" DFA. This sacrifices search performance in
favor of (potentially much) smaller DFAs. One can also generate a "dense" DFA
to get faster searches but larger DFAs.
* Above, we generated a "dfa," but one can also generate a "regex." The
difference is that a DFA can only find the end of a match (or start of a match
if the DFA is reversed), where as a regex will generate two DFAs: one for
finding the end of a match and then another for finding the start. One can
generate two DFAs manually and stitch them together in the code, but generating
a `regex` will take care of this for you.
