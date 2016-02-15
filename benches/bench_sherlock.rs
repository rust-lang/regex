// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use regex::Regex;
use test::Bencher;

lazy_static! {
    static ref SHERLOCK: String = {
        include_str!("the-adventures-of-sherlock-holmes.txt").to_owned()
    };
}

macro_rules! bench_find {
    ($name:ident, $re:expr, $count:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            lazy_static! {
                static ref RE: Regex = $re;
            };
            b.bytes = SHERLOCK.len() as u64;
            b.iter(|| {
                let count = RE.find_iter(&SHERLOCK).count();
                assert_eq!($count, count)
            });
        }
    }
}

// These patterns are all single string literals that compile down to a variant
// of Boyer-Moore w/ memchr. This also demonstrates the impact that the
// frequency of a match has on performance.
bench_find!(name_sherlock, regex!("Sherlock"), 97);
bench_find!(name_holmes, regex!("Holmes"), 461);
bench_find!(name_sherlock_holmes, regex!("Sherlock Holmes"), 91);

// Like the above, except case insensitively.
// The prefix finder is broken at the moment, so these are probably a touch
// slower than they should be.
bench_find!(name_sherlock_nocase, regex!("(?i)Sherlock"), 102);
bench_find!(name_holmes_nocase, regex!("(?i)Holmes"), 467);
bench_find!(name_sherlock_holmes_nocase, regex!("(?i)Sherlock Holmes"), 96);

// Will quickly find instances of 'Sherlock', but then needs to fall back to
// the lazy DFA to process the Unicode aware `\s`.
bench_find!(name_whitespace, regex!(r"Sherlock\s+Holmes"), 97);

// Now try more variations on name matching.
// This one has two alternates that both start with 'S'. This should compile
// to an Aho-Corasick automaton that uses memchr. Never enters lazy DFA.
bench_find!(name_alt1, regex!("Sherlock|Street"), 158);
// This one doesn't have a common byte, but should still use Aho-Corasick.
// Never enters lazy DFA.
bench_find!(name_alt2, regex!("Sherlock|Holmes"), 558);
// OK, still using Aho-Corasick, but more patterns. Never enters lazy DFA.
bench_find!(
    name_alt3,
    regex!("Sherlock|Holmes|Watson|Irene|Adler|John|Baker"), 740);
// Still using Aho-Corasick, but needs the lazy DFA.
bench_find!(
    name_alt3_nocase,
    regex!("(?i)Sherlock|Holmes|Watson|Irene|Adler|John|Baker"), 753);
// Should still use Aho-Corasick for the prefixes in each alternate, but
// we need to use the lazy DFA to complete it.
bench_find!(name_alt4, regex!("Sher[a-z]+|Hol[a-z]+"), 582);
bench_find!(name_alt4_nocase, regex!("(?i)Sher[a-z]+|Hol[a-z]+"), 697);

// How long does it take to discover that there's no match?
// If it starts with an uncommon character, then not long at all.
// A common character? It might be 25x slower!
bench_find!(no_match_uncommon, regex!("zyx"), 0);
bench_find!(no_match_common, regex!("ayx"), 0);

// Various twiddling on very common words.
bench_find!(the_lower, regex!("the"), 7218);
bench_find!(the_upper, regex!("The"), 741);
bench_find!(the_nocase, regex!("(?i)the"), 7987);

// How fast can we match everything? This essentially defeats any clever prefix
// tricks and just executes the DFA across the entire input.
bench_find!(everything_greedy, regex!(".*"), 13053);
bench_find!(everything_greedy_nl, regex!("(?s).*"), 1);

// How fast can we match every letter? This also defeats any clever prefix
// tricks.
bench_find!(letters, regex!(r"\pL"), 447160);
bench_find!(letters_upper, regex!(r"\p{Lu}"), 14180);
bench_find!(letters_lower, regex!(r"\p{Ll}"), 432980);

// Similarly, for words.
bench_find!(words, regex!(r"\w+"), 109214);

// Process whitespace after a very common word.
// Uses Boyer-Moore to find `the` and the lazy DFA for the rest.
bench_find!(the_whitespace, regex!(r"the\s+\w+"), 5410);

// Find complete words before Holmes. The `\w` defeats any prefix
// optimizations, so it's the lazy DFA the entire way.
bench_find!(before_holmes, regex!(r"\w+\s+Holmes"), 319);

// Find Holmes co-occuring with Watson in a particular window of characters.
// This uses Aho-Corasick for the Holmes|Watson prefix, but the lazy DFA for
// the rest.
bench_find!(
    holmes_cochar_watson,
    regex!(r"Holmes.{0,25}Watson|Watson.{0,25}Holmes"), 7);

// Find Holmes co-occuring with Watson in a particular window of words.
// This uses Aho-Corasick for the Holmes|Watson prefix, but the lazy DFA for
// the rest.
bench_find!(
    holmes_coword_watson,
    regex!(r"Holmes(?:\s*.+\s*){0,10}Watson|Watson(?:\s*.+\s*){0,10}Holmes"),
    51);

// Find some subset of quotes in the text.
// This does detect the `"` or `'` prefix literal and does a quick scan for
// either byte before starting the lazy DFA.
bench_find!(quotes, regex!(r#"["'][^"']{0,30}[?!.]["']"#), 767);

// Finds all occurrences of Sherlock Holmes at the beginning or end of a line.
// The empty assertions defeat any detection of prefix literals, so it's the
// lazy DFA the entire way.
bench_find!(
    line_boundary_sherlock_holmes,
    regex!(r"(?m)^Sherlock Holmes|Sherlock Holmes$"), 34);

// All words ending in `n`.
// This uses word boundaries, which the lazy DFA cannot handle. Since the word
// boundary also defeats finding any literal prefixes, we have to use the
// NFA algorithm the whole way.
bench_find!(word_ending_n, regex!(r"\b\w+n\b"), 8366);
