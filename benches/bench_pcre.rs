// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(test)]
#![allow(non_snake_case)]

extern crate enum_set;
#[macro_use] extern crate lazy_static;
extern crate pcre;
extern crate rand;
extern crate regex;
extern crate regex_syntax;
extern crate test;

use std::cell::RefCell;
use std::iter::repeat;

use enum_set::EnumSet;
use pcre::{Pcre, StudyOption};
use rand::{Rng, thread_rng};
use test::Bencher;

/// A nominal wrapper around pcre::Pcre to expose an interface similar to
/// regex::Regex.
struct RegexPcre(RefCell<pcre::Pcre>);

/// lazy_static wants this. No reason not to provide it.
unsafe impl Sync for RegexPcre {}

impl RegexPcre {
    fn is_match(&self, text: &str) -> bool {
        let mut re = self.0.borrow_mut();
        re.exec(text).is_some()
    }
}

macro_rules! regex(
    ($re:expr) => {{
        let mut re = Pcre::compile($re).unwrap();
        let mut opts = EnumSet::new();
        opts.insert(StudyOption::StudyJitCompile);
        re.study_with_options(&opts);
        ::RegexPcre(RefCell::new(re))
    }}
);

fn bench_assert_match(b: &mut Bencher, re: RegexPcre, text: &str) {
    b.iter(|| if !re.is_match(text) { panic!("no match") });
}

fn bench_assert_match2(b: &mut Bencher, re: &RegexPcre, text: &str) {
    b.iter(|| if !re.is_match(text) { panic!("no match") });
}

// We can't run this with PCRE for obvious reasons.
// Interestingly, if we try, the rust-pcre crate panics because it doesn't
// recognize an error returned by PCRE.
/*
#[bench]
fn no_exponential(b: &mut Bencher) {
    let n = 100;
    let regex_string = format!(
        "{}{}",
        repeat("a?").take(n).collect::<String>(),
        repeat("a").take(n).collect::<String>());
    let re = regex!(&regex_string);
    let text: String = repeat("a").take(n).collect();
    bench_assert_match(b, re, &text);
}
*/

#[bench]
fn literal(b: &mut Bencher) {
    let re = regex!("y");
    let text = format!("{}y", repeat("x").take(50).collect::<String>());
    bench_assert_match(b, re, &text);
}

#[bench]
fn not_literal(b: &mut Bencher) {
    let re = regex!(".y");
    let text = format!("{}y", repeat("x").take(50).collect::<String>());
    bench_assert_match(b, re, &text);
}

#[bench]
fn match_class(b: &mut Bencher) {
    let re = regex!("[abcdw]");
    let text = format!("{}w", repeat("xxxx").take(20).collect::<String>());
    bench_assert_match(b, re, &text);
}

#[bench]
fn match_class_in_range(b: &mut Bencher) {
    // 'b' is between 'a' and 'c', so the class range checking doesn't help.
    let re = regex!("[ac]");
    let text = format!("{}c", repeat("bbbb").take(20).collect::<String>());
    bench_assert_match(b, re, &text);
}

#[bench]
fn match_class_unicode(b: &mut Bencher) {
    let re = regex!(r"\pL");
    let text = format!("{}a", repeat("☃5☃5").take(20).collect::<String>());
    bench_assert_match2(b, &re, &text);
}

#[bench]
fn anchored_literal_short_non_match(b: &mut Bencher) {
    let re = regex!("^zbc(d|e)");
    let text = "abcdefghijklmnopqrstuvwxyz";
    b.iter(|| re.is_match(text));
}

#[bench]
fn anchored_literal_long_non_match(b: &mut Bencher) {
    let re = regex!("^zbc(d|e)");
    let text: String = repeat("abcdefghijklmnopqrstuvwxyz").take(15).collect();
    b.iter(|| re.is_match(&text));
}

#[bench]
fn anchored_literal_short_match(b: &mut Bencher) {
    let re = regex!("^.bc(d|e)");
    let text = "abcdefghijklmnopqrstuvwxyz";
    b.iter(|| re.is_match(text));
}

#[bench]
fn anchored_literal_long_match(b: &mut Bencher) {
    let re = regex!("^.bc(d|e)");
    let text: String = repeat("abcdefghijklmnopqrstuvwxyz").take(15).collect();
    b.iter(|| re.is_match(&text));
}

#[bench]
fn one_pass_short_a(b: &mut Bencher) {
    let re = regex!("^.bc(d|e)*$");
    let text = "abcddddddeeeededd";
    b.iter(|| re.is_match(text));
}

#[bench]
fn one_pass_short_a_not(b: &mut Bencher) {
    let re = regex!(".bc(d|e)*$");
    let text = "abcddddddeeeededd";
    b.iter(|| re.is_match(text));
}

#[bench]
fn one_pass_short_b(b: &mut Bencher) {
    let re = regex!("^.bc(?:d|e)*$");
    let text = "abcddddddeeeededd";
    b.iter(|| re.is_match(text));
}

#[bench]
fn one_pass_short_b_not(b: &mut Bencher) {
    let re = regex!(".bc(?:d|e)*$");
    let text = "abcddddddeeeededd";
    b.iter(|| re.is_match(text));
}

#[bench]
fn one_pass_long_prefix(b: &mut Bencher) {
    let re = regex!("^abcdefghijklmnopqrstuvwxyz.*$");
    let text = "abcdefghijklmnopqrstuvwxyz";
    b.iter(|| re.is_match(text));
}

#[bench]
fn one_pass_long_prefix_not(b: &mut Bencher) {
    let re = regex!("^.bcdefghijklmnopqrstuvwxyz.*$");
    let text = "abcdefghijklmnopqrstuvwxyz";
    b.iter(|| re.is_match(text));
}

macro_rules! throughput(
    ($name:ident, $regex:expr, $size:expr) => (
        #[bench]
        fn $name(b: &mut Bencher) {
            lazy_static! {
                static ref RE: RegexPcre = $regex;
                static ref TEXT: String = gen_text($size);
            };
            // let re = $regex;
            // let text = gen_text($size);
            b.bytes = $size;
            b.iter(|| if RE.is_match(&TEXT) { panic!("match") });
        }
    );
);

fn easy0() -> RegexPcre { regex!("ABCDEFGHIJKLMNOPQRSTUVWXYZ$") }
fn easy1() -> RegexPcre { regex!("A[AB]B[BC]C[CD]D[DE]E[EF]F[FG]G[GH]H[HI]I[IJ]J$") }
fn medium() -> RegexPcre { regex!("[XYZ]ABCDEFGHIJKLMNOPQRSTUVWXYZ$") }
fn hard() -> RegexPcre { regex!("[ -~]*ABCDEFGHIJKLMNOPQRSTUVWXYZ$") }

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

throughput!(easy0_32, easy0(), 32);
throughput!(easy0_1K, easy0(), 1<<10);
throughput!(easy0_32K, easy0(), 32<<10);
throughput!(easy0_1MB, easy0(), 1<<20);

throughput!(easy1_32, easy1(), 32);
throughput!(easy1_1K, easy1(), 1<<10);
throughput!(easy1_32K, easy1(), 32<<10);
throughput!(easy1_1MB, easy1(), 1<<20);

throughput!(medium_32, medium(), 32);
throughput!(medium_1K, medium(), 1<<10);
throughput!(medium_32K,medium(), 32<<10);
throughput!(medium_1MB, medium(), 1<<20);

throughput!(hard_32, hard(), 32);
throughput!(hard_1K, hard(), 1<<10);
throughput!(hard_32K,hard(), 32<<10);
throughput!(hard_1MB, hard(), 1<<20);
