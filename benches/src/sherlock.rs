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

use Regex;

#[cfg(not(feature = "re-rust-bytes"))]
lazy_static! {
    static ref SHERLOCK: String = {
        include_str!("the-adventures-of-sherlock-holmes.txt").to_owned()
    };
}

#[cfg(feature = "re-rust-bytes")]
lazy_static! {
    static ref SHERLOCK: Vec<u8> = {
        include_bytes!("the-adventures-of-sherlock-holmes.txt")[..].to_owned()
    };
}

macro_rules! bench_find {
    ($name:ident, $re:expr, $count:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            #![allow(unused_mut)]

            use std::sync::Mutex;

            lazy_static! {
                static ref RE: Mutex<Regex> = Mutex::new($re);
            };
            let mut re = RE.lock().unwrap();
            b.bytes = SHERLOCK.len() as u64;
            b.iter(|| {
                let count = re.find_iter(&SHERLOCK).count();
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

// Like the above, except case insensitively. The prefix detector will extract
// multiple *cut* prefix literals for each of the following before hitting its
// limit. All of these should be able to use either memchr2 or memchr3.
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
// This one doesn't have a common byte, but should still use Aho-Corasick and
// memchr2.
// Never enters lazy DFA.
bench_find!(name_alt2, regex!("Sherlock|Holmes"), 558);
// Still using Aho-Corasick, but more patterns. Never enters lazy DFA but
// also can't use any memchr variant.
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
// Uses Aho-Corasick, but can use memchr3 (unlike name_alt3).
bench_find!(name_alt5, regex!("Sherlock|Holmes|Watson"), 639);
bench_find!(name_alt5_nocase, regex!("(?i)Sherlock|Holmes|Watson"), 650);

// How long does it take to discover that there's no match? In the first two
// cases, we detect the rarest byte in the literal to run memchr on. In the
// first, it's 'z' and in the second it's 'j'. The third case only has common
// letters, and is therefore slower.
bench_find!(no_match_uncommon, regex!("zqj"), 0);
bench_find!(no_match_common, regex!("aqj"), 0);
bench_find!(no_match_really_common, regex!("aei"), 0);

// Various twiddling on very common words. This tends to stress the constant
// overhead of actually reporting a match. (None of these actually enter any
// matching engines.)
bench_find!(the_lower, regex!("the"), 7218);
bench_find!(the_upper, regex!("The"), 741);
bench_find!(the_nocase, regex!("(?i)the"), 7987);

// Process whitespace after a very common word.
// Uses Boyer-Moore to find `the` and the lazy DFA for the rest.
bench_find!(the_whitespace, regex!(r"the\s+\w+"), 5410);

// How fast can we match everything? This essentially defeats any clever prefix
// tricks and just executes the DFA across the entire input.
#[cfg(not(feature = "re-pcre"))]
#[cfg(not(feature = "re-pcre2"))]
bench_find!(everything_greedy, regex!(".*"), 13053);
#[cfg(not(feature = "re-onig"))]
#[cfg(not(feature = "re-pcre"))]
#[cfg(not(feature = "re-pcre2"))]
bench_find!(everything_greedy_nl, regex!("(?s).*"), 1);

// How fast can we match every letter? This also defeats any clever prefix
// tricks. Weird. Looks like PCRE2 diverges. Not clear who is right...
#[cfg(not(feature = "re-rust-bytes"))]
bench_find!(letters, regex!(r"\p{L}"), 447160);
#[cfg(feature = "re-rust-bytes")]
bench_find!(letters, regex!(r"(?u)\p{L}"), 447160);

#[cfg(not(feature = "re-rust-bytes"))]
bench_find!(letters_upper, regex!(r"\p{Lu}"), 14180);
#[cfg(feature = "re-rust-bytes")]
bench_find!(letters_upper, regex!(r"(?u)\p{Lu}"), 14180);

#[cfg(not(feature = "re-rust-bytes"))]
bench_find!(letters_lower, regex!(r"\p{Ll}"), 432980);
#[cfg(feature = "re-rust-bytes")]
bench_find!(letters_lower, regex!(r"(?u)\p{Ll}"), 432980);

// Similarly, for words.
#[cfg(not(feature = "re-rust-bytes"))]
bench_find!(words, regex!(r"\w+"), 109214);
#[cfg(feature = "re-rust-bytes")]
bench_find!(words, regex!(r"(?u)\w+"), 109214);

// Find complete words before Holmes. The `\w` defeats any prefix
// optimizations, but 'Holmes' triggers the reverse suffix optimization.
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
#[cfg(not(feature = "re-onig"))]
#[cfg(not(feature = "re-pcre"))]
#[cfg(not(feature = "re-pcre2"))]
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
// NFA algorithm the whole way, which is quite slow.
//
// Unless we're using bytes::Regex, which will use an ASCII word boundary,
// which the DFA can indeed handle.
bench_find!(word_ending_n, regex!(r"\b\w+n\b"), 8366);

// This is a real bad one for Rust's engine. This particular expression
// fills the state cache quite frequently, which results in a lot of churn.
// This can be made to go roughly the speed of PCRE by increasing the DFA cache
// size.
//
// Its only salvation is that the DFA realizes it's executing slowly, gives up
// quickly and falls back to the NFA algorithm.
bench_find!(repeated_class_negation, regex!(r"[a-q][^u-z]{13}x"), 142);

// This defeats any prefix optimizations but triggers the reverse suffix
// optimization.
bench_find!(ing_suffix, regex!(r"[a-zA-Z]+ing"), 2824);

// Similar to ing_suffix, but a little more complex by limiting the length
// of the word and making sure it's surrounded by whitespace. The trailing
// `\s` defeats the reverse suffix optimization.
//
// Onig does surprisingly well on this benchmark and yet does quite poorly on
// the ing_suffix benchmark. That one has me stumped.
//
// Interestingly, this is slower in the rust-bytes benchmark, presumably
// because scanning for one of the bytes in the Unicode *unaware* `\s` ends
// up being slower than avoiding the prefix scan at all.
bench_find!(ing_suffix_limited_space, regex!(r"\s[a-zA-Z]{0,12}ing\s"), 2081);
