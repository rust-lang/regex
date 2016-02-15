// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// These benchmarks use PCRE to reproduce some of the benchmarks used to track
// performance of regexes in this crate. I'm not an experienced user of PCRE,
// so it's possible that usage here is not optimal. If it isn't, then
// improvements are welcome. (I'm aware that PCRE has a DFA, but it doesn't
// seem to actually preserve leftmost-first semantics, like the DFA in this
// crate does.)
//
// Note that for these benchmarks, all we need is to detect whether there is
// a match or not.

#![feature(test)]
#![allow(non_snake_case)]

extern crate enum_set;
#[macro_use] extern crate lazy_static;
extern crate pcre;
extern crate rand;
extern crate regex;
extern crate test;

use std::iter::repeat;

use rand::{Rng, thread_rng};
use test::Bencher;

/// A nominal wrapper around pcre::Pcre to expose an interface similar to
/// regex::Regex.
struct RegexPcre(pcre::Pcre);

/// lazy_static wants this. No reason not to provide it.
unsafe impl Send for RegexPcre {}
unsafe impl Sync for RegexPcre {}

impl RegexPcre {
    fn is_match(&mut self, text: &str) -> bool {
        self.0.exec(text).is_some()
    }

    fn count_matches(&mut self, text: &str) -> usize {
        self.0.matches(text).count()
    }
}

macro_rules! regex(
    ($re:expr) => {{
        use enum_set::EnumSet;
        use pcre::{Pcre, CompileOption, StudyOption};

        let mut comp_opts = EnumSet::new();
        // Rust's regex library exclusively uses Unicode-aware character
        // classes.
        comp_opts.insert(CompileOption::Ucp);
        let mut re = Pcre::compile_with_options($re, &comp_opts).unwrap();

        let mut study_opts = EnumSet::new();
        study_opts.insert(StudyOption::StudyJitCompile);
        re.study_with_options(&study_opts);

        ::RegexPcre(re)
    }}
);

macro_rules! bench_match {
    ($name:ident, $re:expr, $text:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            use std::sync::Mutex;

            lazy_static! {
                static ref RE: Mutex<RegexPcre> = Mutex::new($re);
                static ref TEXT: String = $text;
            };
            let mut re = RE.lock().unwrap();
            b.bytes = TEXT.len() as u64;
            b.iter(|| {
                if !re.is_match(&TEXT) {
                    panic!("expected match, got not match");
                }
            });
        }
    }
}

macro_rules! bench_nomatch {
    ($name:ident, $re:expr, $text:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            use std::sync::Mutex;

            lazy_static! {
                static ref RE: Mutex<RegexPcre> = Mutex::new($re);
                static ref TEXT: String = $text;
            };
            let mut re = RE.lock().unwrap();
            b.bytes = TEXT.len() as u64;
            b.iter(|| {
                if re.is_match(&TEXT) {
                    panic!("match not expected");
                }
            });
        }
    }
}

bench_match!(literal, regex!("y"), {
   format!("{}y", repeat("x").take(50).collect::<String>())
});

bench_match!(not_literal, regex!(".y"), {
   format!("{}y", repeat("x").take(50).collect::<String>())
});

bench_match!(match_class, regex!("[abcdw]"), {
    format!("{}w", repeat("xxxx").take(20).collect::<String>())
});

bench_match!(match_class_in_range, regex!("[ac]"), {
    format!("{}c", repeat("bbbb").take(20).collect::<String>())
});

bench_match!(match_class_unicode, regex!(r"\pL"), {
    format!("{}a", repeat("☃5☃5").take(20).collect::<String>())
});

bench_nomatch!(anchored_literal_short_non_match, regex!("^zbc(d|e)"), {
    "abcdefghijklmnopqrstuvwxyz".to_owned()
});

bench_nomatch!(anchored_literal_long_non_match, regex!("^zbc(d|e)"), {
    repeat("abcdefghijklmnopqrstuvwxyz").take(15).collect::<String>()
});

bench_match!(anchored_literal_short_match, regex!("^.bc(d|e)"), {
    "abcdefghijklmnopqrstuvwxyz".to_owned()
});

bench_match!(anchored_literal_long_match, regex!("^.bc(d|e)"), {
    repeat("abcdefghijklmnopqrstuvwxyz").take(15).collect::<String>()
});

bench_match!(one_pass_short, regex!("^.bc(d|e)*$"), {
    "abcddddddeeeededd".to_owned()
});

bench_match!(one_pass_short_not, regex!(".bc(d|e)*$"), {
    "abcddddddeeeededd".to_owned()
});

bench_match!(one_pass_long_prefix, regex!("^abcdefghijklmnopqrstuvwxyz.*$"), {
    "abcdefghijklmnopqrstuvwxyz".to_owned()
});

bench_match!(one_pass_long_prefix_not, regex!("^.bcdefghijklmnopqrstuvwxyz.*$"), {
    "abcdefghijklmnopqrstuvwxyz".to_owned()
});

fn gen_text(n: usize) -> String {
    let mut rng = thread_rng();
    let mut bytes = rng.gen_ascii_chars().map(|n| n as u8).take(n)
                       .collect::<Vec<u8>>();
    for (i, b) in bytes.iter_mut().enumerate() {
        if i % 20 == 0 {
            *b = b'\n'
        }
    }
    String::from_utf8(bytes).unwrap()
}

fn easy0() -> RegexPcre {
    regex!("ABCDEFGHIJKLMNOPQRSTUVWXYZ$")
}

bench_nomatch!(easy0_32, easy0(), gen_text(32));
bench_nomatch!(easy0_1K, easy0(), gen_text(1<<10));
bench_nomatch!(easy0_32K, easy0(), gen_text(32<<10));
bench_nomatch!(easy0_1MB, easy0(), gen_text(1<<20));

fn easy1() -> RegexPcre {
    regex!("A[AB]B[BC]C[CD]D[DE]E[EF]F[FG]G[GH]H[HI]I[IJ]J$")
}

bench_nomatch!(easy1_32, easy1(), gen_text(32));
bench_nomatch!(easy1_1K, easy1(), gen_text(1<<10));
bench_nomatch!(easy1_32K, easy1(), gen_text(32<<10));
bench_nomatch!(easy1_1MB, easy1(), gen_text(1<<20));

fn medium() -> RegexPcre {
    regex!("[XYZ]ABCDEFGHIJKLMNOPQRSTUVWXYZ$")
}

bench_nomatch!(medium_32, medium(), gen_text(32));
bench_nomatch!(medium_1K, medium(), gen_text(1<<10));
bench_nomatch!(medium_32K, medium(), gen_text(32<<10));
bench_nomatch!(medium_1MB, medium(), gen_text(1<<20));

fn hard() -> RegexPcre {
    regex!("[ -~]*ABCDEFGHIJKLMNOPQRSTUVWXYZ$")
}

bench_nomatch!(hard_32, hard(), gen_text(32));
bench_nomatch!(hard_1K, hard(), gen_text(1<<10));
bench_nomatch!(hard_32K, hard(), gen_text(32<<10));
bench_nomatch!(hard_1MB, hard(), gen_text(1<<20));


// These are the Sherlock Holmes benchmarks. Not all of them are present
// since the syntax isn't exactly the same for things like case insensitive
// matching. We could add them back by twiddling the flags using PCRE though.
//
// Other benchmarks are removed purely because PCRE is too darn slow on them.

mod sherlock {
    use super::RegexPcre;
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
                use std::sync::Mutex;

                lazy_static! {
                    static ref RE: Mutex<RegexPcre> = Mutex::new($re);
                };
                let mut re = RE.lock().unwrap();
                b.bytes = SHERLOCK.len() as u64;
                b.iter(|| {
                    let count = re.count_matches(&SHERLOCK);
                    assert_eq!($count, count)
                });
            }
        }
    }

    bench_find!(name_sherlock, regex!("Sherlock"), 97);
    bench_find!(name_holmes, regex!("Holmes"), 461);
    bench_find!(name_sherlock_holmes, regex!("Sherlock Holmes"), 91);

    bench_find!(name_sherlock_nocase, regex!("(?i)Sherlock"), 102);
    bench_find!(name_holmes_nocase, regex!("(?i)Holmes"), 467);
    bench_find!(
        name_sherlock_holmes_nocase, regex!("(?i)Sherlock Holmes"), 96);

    bench_find!(name_whitespace, regex!(r"Sherlock\s+Holmes"), 97);

    bench_find!(name_alt1, regex!("Sherlock|Street"), 158);
    bench_find!(name_alt2, regex!("Sherlock|Holmes"), 558);
    bench_find!(
        name_alt3,
        regex!("Sherlock|Holmes|Watson|Irene|Adler|John|Baker"), 740);
    bench_find!(
        name_alt3_nocase,
        regex!("(?i)Sherlock|Holmes|Watson|Irene|Adler|John|Baker"), 753);
    bench_find!(name_alt4, regex!("Sher[a-z]+|Hol[a-z]+"), 582);
    bench_find!(name_alt4_nocase, regex!("(?i)Sher[a-z]+|Hol[a-z]+"), 697);

    bench_find!(no_match_uncommon, regex!("zyx"), 0);
    bench_find!(no_match_common, regex!("ayx"), 0);

    bench_find!(the_lower, regex!("the"), 7218);
    bench_find!(the_upper, regex!("The"), 741);
    bench_find!(the_nocase, regex!("(?i)the"), 7987);

    // bench_find!(everything_greedy, regex!(".*"), 13053);
    // bench_find!(everything_greedy_nl, regex!("(?s).*"), 1);

    bench_find!(letters, regex!(r"\pL"), 447160);
    bench_find!(letters_upper, regex!(r"\p{Lu}"), 14180);
    bench_find!(letters_lower, regex!(r"\p{Ll}"), 432980);

    bench_find!(words, regex!(r"\w+"), 109214);

    bench_find!(the_whitespace, regex!(r"the\s+\w+"), 5410);

    bench_find!(before_holmes, regex!(r"\w+\s+Holmes"), 319);

    bench_find!(holmes_cochar_watson,
                regex!(r"Holmes.{0,25}Watson|Watson.{0,25}Holmes"), 7);

    // bench_find!(
        // holmes_coword_watson,
        // regex!(r"Holmes(?:\s*.+\s*){0,10}Watson|Watson(?:\s*.+\s*){0,10}Holmes"),
        // 64);

    bench_find!(quotes, regex!(r#"["'][^"']{0,30}[?!.]["']"#), 767);

    bench_find!(line_boundary_sherlock_holmes,
                regex!(r"(?m)^Sherlock Holmes|Sherlock Holmes$"), 34);

    bench_find!(word_ending_n, regex!(r"\b\w+n\b"), 8366);
}
