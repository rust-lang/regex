// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use test::Bencher;

use {Regex, Text};


// USAGE: sherlock!(name, pattern, count)
//
// This is same as bench_find, except it always uses the Sherlock haystack.
macro_rules! sherlock {
    ($name:ident, $pattern:expr, $count:expr) => {
        bench_find!(
            $name, $pattern, $count,
            include_str!("data/sherlock.txt").to_owned()
        );
    }
}

// These patterns are all single string literals that compile down to a variant
// of Boyer-Moore w/ memchr. This also demonstrates the impact that the
// frequency of a match has on performance.
sherlock!(name_sherlock, r"Sherlock", 97);
sherlock!(name_holmes, r"Holmes", 461);
sherlock!(name_sherlock_holmes, r"Sherlock Holmes", 91);

// Like the above, except case insensitively. The prefix detector will extract
// multiple *cut* prefix literals for each of the following before hitting its
// limit. All of these should be able to use either memchr2 or memchr3.
sherlock!(name_sherlock_nocase, r"(?i)Sherlock", 102);
sherlock!(name_holmes_nocase, r"(?i)Holmes", 467);
sherlock!(name_sherlock_holmes_nocase, r"(?i)Sherlock Holmes", 96);

// Will quickly find instances of 'Sherlock', but then needs to fall back to
// the lazy DFA to process the Unicode aware `\s`.
sherlock!(name_whitespace, r"Sherlock\s+Holmes", 97);

// Now try more variations on name matching.
// This one has two alternates that both start with 'S'. This should compile
// to an Aho-Corasick automaton that uses memchr. Never enters lazy DFA.
sherlock!(name_alt1, r"Sherlock|Street", 158);
// This one doesn't have a common byte, but should still use Aho-Corasick and
// memchr2.
// Never enters lazy DFA.
sherlock!(name_alt2, r"Sherlock|Holmes", 558);
// Still using Aho-Corasick, but more patterns. Never enters lazy DFA but
// also can't use any memchr variant.
sherlock!(name_alt3, r"Sherlock|Holmes|Watson|Irene|Adler|John|Baker", 740);
// Still using Aho-Corasick, but needs the lazy DFA.
sherlock!(
    name_alt3_nocase,
    r"(?i)Sherlock|Holmes|Watson|Irene|Adler|John|Baker",
    753);
// Should still use Aho-Corasick for the prefixes in each alternate, but
// we need to use the lazy DFA to complete it.
sherlock!(name_alt4, r"Sher[a-z]+|Hol[a-z]+", 582);
sherlock!(name_alt4_nocase, r"(?i)Sher[a-z]+|Hol[a-z]+", 697);
// Uses Aho-Corasick, but can use memchr3 (unlike name_alt3).
sherlock!(name_alt5, r"Sherlock|Holmes|Watson", 639);
sherlock!(name_alt5_nocase, r"(?i)Sherlock|Holmes|Watson", 650);

// How long does it take to discover that there's no match? In the first two
// cases, we detect the rarest byte in the literal to run memchr on. In the
// first, it's 'z' and in the second it's 'j'. The third case only has common
// letters, and is therefore slower.
sherlock!(no_match_uncommon, r"zqj", 0);
sherlock!(no_match_common, r"aqj", 0);
sherlock!(no_match_really_common, r"aei", 0);

// Various twiddling on very common words. This tends to stress the constant
// overhead of actually reporting a match. (None of these actually enter any
// matching engines.)
sherlock!(the_lower, r"the", 7218);
sherlock!(the_upper, r"The", 741);
sherlock!(the_nocase, r"(?i)the", 7987);

// Process whitespace after a very common word.
// Uses Boyer-Moore to find `the` and the lazy DFA for the rest.
sherlock!(the_whitespace, r"the\s+\w+", 5410);

// How fast can we match everything? This essentially defeats any clever prefix
// tricks and just executes the DFA across the entire input.
#[cfg(not(feature = "re-pcre1"))]
#[cfg(not(feature = "re-pcre2"))]
#[cfg(not(feature = "re-tcl"))]
sherlock!(everything_greedy, r".*", 13053);
#[cfg(not(feature = "re-onig"))]
#[cfg(not(feature = "re-pcre1"))]
#[cfg(not(feature = "re-pcre2"))]
#[cfg(not(feature = "re-tcl"))]
sherlock!(everything_greedy_nl, r"(?s).*", 1);

// How fast can we match every letter? This also defeats any clever prefix
// tricks.
#[cfg(not(feature = "re-tcl"))]
sherlock!(letters, r"\p{L}", 447160);

#[cfg(not(feature = "re-tcl"))]
sherlock!(letters_upper, r"\p{Lu}", 14180);

#[cfg(not(feature = "re-tcl"))]
sherlock!(letters_lower, r"\p{Ll}", 432980);

// Similarly, for words.
#[cfg(not(feature = "re-re2"))]
sherlock!(words, r"\w+", 109214);
#[cfg(feature = "re-re2")]
sherlock!(words, r"\w+", 109222); // hmm, why does RE2 diverge here?

// Find complete words before Holmes. The `\w` defeats any prefix
// optimizations.
sherlock!(before_holmes, r"\w+\s+Holmes", 319);

// Find complete words before Holmes. Both of the `\w`s defeat any prefix
// and suffix optimizations.
sherlock!(before_after_holmes, r"\w+\s+Holmes\s+\w+", 137);

// Find Holmes co-occuring with Watson in a particular window of characters.
// This uses Aho-Corasick for the Holmes|Watson prefix, but the lazy DFA for
// the rest.
sherlock!(holmes_cochar_watson, r"Holmes.{0,25}Watson|Watson.{0,25}Holmes", 7);

// Find Holmes co-occuring with Watson in a particular window of words.
// This uses Aho-Corasick for the Holmes|Watson prefix, but the lazy DFA for
// the rest.
#[cfg(not(feature = "re-onig"))]
#[cfg(not(feature = "re-pcre1"))]
#[cfg(not(feature = "re-pcre2"))]
#[cfg(not(feature = "re-tcl"))]
sherlock!(
    holmes_coword_watson,
    r"Holmes(?:\s*.+\s*){0,10}Watson|Watson(?:\s*.+\s*){0,10}Holmes",
    51);

// Find some subset of quotes in the text.
// This does detect the `"` or `'` prefix literal and does a quick scan for
// either byte before starting the lazy DFA.
sherlock!(quotes, r#"["'][^"']{0,30}[?!.]["']"#, 767);

// Finds all occurrences of Sherlock Holmes at the beginning or end of a line.
// The empty assertions defeat any detection of prefix literals, so it's the
// lazy DFA the entire way.
sherlock!(
    line_boundary_sherlock_holmes,
    r"(?m)^Sherlock Holmes|Sherlock Holmes$",
    34);

// All words ending in `n`. This uses Unicode word boundaries, which the DFA
// can speculatively handle. Since this benchmark is on mostly ASCII text, it
// performs well here. A different benchmark with non-Western text would be
// more revealing since the search would be forced to fall back to an NFA
// simulation.
#[cfg(not(feature = "re-tcl"))]
sherlock!(word_ending_n, r"\b\w+n\b", 8366);

// This is a real bad one for Rust's engine. This particular expression
// fills the state cache quite frequently, which results in a lot of churn.
// This can be made to go roughly the speed of PCRE by increasing the DFA cache
// size.
//
// Its only salvation is that the DFA realizes it's executing slowly, gives up
// quickly and falls back to the NFA algorithm.
//
// RE2 seems to do a worse job at this than Rust. So much so that it's slow
// enough to be annoying, so we disable it.
#[cfg(not(feature = "re-re2"))]
sherlock!(repeated_class_negation, r"[a-q][^u-z]{13}x", 142);

// This defeats any prefix optimizations but triggers the reverse suffix
// optimization.
sherlock!(ing_suffix, r"[a-zA-Z]+ing", 2824);

// Similar to ing_suffix, but a little more complex by limiting the length
// of the word and making sure it's surrounded by whitespace. The trailing
// `\s` defeats the reverse suffix optimization.
//
// Onig does surprisingly well on this benchmark and yet does quite poorly on
// the ing_suffix benchmark. That one has me stumped.
sherlock!(ing_suffix_limited_space, r"\s[a-zA-Z]{0,12}ing\s", 2081);
