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

bench_find!(name_sherlock, regex!("Sherlock"), 97);
bench_find!(name_holmes, regex!("Holmes"), 461);
bench_find!(name_sherlock_holmes, regex!("Sherlock Holmes"), 91);

bench_find!(name_sherlock_nocase, regex!("(?i)Sherlock"), 102);
bench_find!(name_holmes_nocase, regex!("(?i)Holmes"), 467);
bench_find!(name_sherlock_holmes_nocase, regex!("(?i)Sherlock Holmes"), 96);

bench_find!(name_whitespace, regex!(r"Sherlock\s+Holmes"), 97);

bench_find!(name_alt1, regex!("Sherlock|Street"), 158);
bench_find!(name_alt2, regex!("Sherlock|Holmes"), 558);
bench_find!(name_alt3,
            regex!("Sherlock|Holmes|Watson|Irene|Adler|John|Baker"), 740);
bench_find!(name_alt3_nocase,
            regex!("(?i)Sherlock|Holmes|Watson|Irene|Adler|John|Baker"), 753);
bench_find!(name_alt4, regex!("Sher[a-z]+|Hol[a-z]+"), 582);
bench_find!(name_alt4_nocase, regex!("(?i)Sher[a-z]+|Hol[a-z]+"), 697);

bench_find!(no_match_uncommon, regex!("zyx"), 0);
bench_find!(no_match_common, regex!("ayx"), 0);

bench_find!(the_lower, regex!("the"), 7218);
bench_find!(the_upper, regex!("The"), 741);
bench_find!(the_nocase, regex!("(?i)the"), 7987);

bench_find!(everything_greedy, regex!(".*"), 13053);
bench_find!(everything_greedy_nl, regex!("(?s).*"), 1);

bench_find!(the_whitespace, regex!(r"the\s+\w+"), 5410);

bench_find!(before_holmes, regex!(r"\w+\s+Holmes"), 319);

bench_find!(holmes_cochar_watson,
            regex!(r"Holmes.{0,25}Watson|Watson.{0,25}Holmes"), 7);

bench_find!(
    holmes_coword_watson,
    regex!(r"Holmes(?:\s*.+\s*){0,25}Watson|Watson(?:\s*.+\s*){0,25}Holmes"),
    64);

bench_find!(quotes, regex!(r#"["'][^"']{0,30}[?!.]["']"#), 767);

bench_find!(line_boundary_sherlock_holmes,
            regex!(r"(?m)^Sherlock Holmes|Sherlock Holmes$"), 34);

bench_find!(word_ending_n, regex!(r"\b\w+n\b"), 8366);
