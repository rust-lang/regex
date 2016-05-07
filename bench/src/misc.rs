// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(non_snake_case)]

use std::iter::repeat;

use test::Bencher;

use {Regex, Text};

#[cfg(not(feature = "re-onig"))]
#[cfg(not(feature = "re-pcre1"))]
#[cfg(not(feature = "re-pcre2"))]
#[cfg(not(feature = "re-rust-plugin"))]
bench_match!(no_exponential, {
    format!(
        "{}{}",
        repeat("a?").take(100).collect::<String>(),
        repeat("a").take(100).collect::<String>())
}, repeat("a").take(100).collect());

bench_match!(literal, r"y", {
   format!("{}y", repeat("x").take(50).collect::<String>())
});

bench_match!(not_literal, r".y", {
   format!("{}y", repeat("x").take(50).collect::<String>())
});

bench_match!(match_class, "[abcdw]", {
    format!("{}w", repeat("xxxx").take(20).collect::<String>())
});

bench_match!(match_class_in_range, "[ac]", {
    format!("{}c", repeat("bbbb").take(20).collect::<String>())
});

#[cfg(not(feature = "re-rust-bytes"))]
#[cfg(not(feature = "re-tcl"))]
bench_match!(match_class_unicode, r"\p{L}", {
    format!("{}a", repeat("☃5☃5").take(20).collect::<String>())
});

bench_not_match!(anchored_literal_short_non_match, r"^zbc(d|e)", {
    "abcdefghijklmnopqrstuvwxyz".to_owned()
});

bench_not_match!(anchored_literal_long_non_match, r"^zbc(d|e)", {
    repeat("abcdefghijklmnopqrstuvwxyz").take(15).collect::<String>()
});

bench_match!(anchored_literal_short_match, r"^.bc(d|e)", {
    "abcdefghijklmnopqrstuvwxyz".to_owned()
});

bench_match!(anchored_literal_long_match, r"^.bc(d|e)", {
    repeat("abcdefghijklmnopqrstuvwxyz").take(15).collect::<String>()
});

bench_match!(one_pass_short, r"^.bc(d|e)*$", {
    "abcddddddeeeededd".to_owned()
});

bench_match!(one_pass_short_not, r".bc(d|e)*$", {
    "abcddddddeeeededd".to_owned()
});

bench_match!(one_pass_long_prefix, r"^abcdefghijklmnopqrstuvwxyz.*$", {
    "abcdefghijklmnopqrstuvwxyz".to_owned()
});

bench_match!(one_pass_long_prefix_not, r"^.bcdefghijklmnopqrstuvwxyz.*$", {
    "abcdefghijklmnopqrstuvwxyz".to_owned()
});

bench_match!(long_needle1, r"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", {
    repeat("a").take(100_000).collect::<String>() + "b"
});

bench_match!(long_needle2, r"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbba", {
    repeat("b").take(100_000).collect::<String>() + "a"
});

// This benchmark specifically targets the "reverse suffix literal"
// optimization. In particular, it is easy for a naive implementation to
// take quadratic worst case time. This benchmark provides a case for such
// a scenario.
bench_not_match!(reverse_suffix_no_quadratic, r"[r-z].*bcdefghijklmnopq", {
    repeat("bcdefghijklmnopq").take(500).collect::<String>()
});

#[cfg(feature = "re-rust")]
#[bench]
fn replace_all(b: &mut Bencher) {
    let re = regex!("[cjrw]");
    let text = "abcdefghijklmnopqrstuvwxyz";
    b.iter(|| re.replace_all(text, ""));
}

const TXT_32: &'static str = include_str!("data/32.txt");
const TXT_1K: &'static str = include_str!("data/1K.txt");
const TXT_32K: &'static str = include_str!("data/32K.txt");
const TXT_1MB: &'static str = include_str!("data/1MB.txt");

fn get_text(corpus: &str, suffix: String) -> String {
    let mut corpus = corpus.to_string();
    corpus.push_str(&suffix);
    corpus
}

fn easy0_suffix() -> String {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ".to_string()
}

macro_rules! easy0 { () => ("ABCDEFGHIJKLMNOPQRSTUVWXYZ$") }

bench_match!(easy0_32, easy0!(), get_text(TXT_32, easy0_suffix()));
bench_match!(easy0_1K, easy0!(), get_text(TXT_1K, easy0_suffix()));
bench_match!(easy0_32K, easy0!(), get_text(TXT_32K, easy0_suffix()));
bench_match!(easy0_1MB, easy0!(), get_text(TXT_1MB, easy0_suffix()));

fn easy1_suffix() -> String {
    "AABCCCDEEEFGGHHHIJJ".to_string()
}

macro_rules! easy1 {
    () => (r"A[AB]B[BC]C[CD]D[DE]E[EF]F[FG]G[GH]H[HI]I[IJ]J$")
}

bench_match!(easy1_32, easy1!(), get_text(TXT_32, easy1_suffix()));
bench_match!(easy1_1K, easy1!(), get_text(TXT_1K, easy1_suffix()));
bench_match!(easy1_32K, easy1!(), get_text(TXT_32K, easy1_suffix()));
bench_match!(easy1_1MB, easy1!(), get_text(TXT_1MB, easy1_suffix()));

fn medium_suffix() -> String {
    "XABCDEFGHIJKLMNOPQRSTUVWXYZ".to_string()
}

macro_rules! medium { () => (r"[XYZ]ABCDEFGHIJKLMNOPQRSTUVWXYZ$") }

bench_match!(medium_32, medium!(), get_text(TXT_32, medium_suffix()));
bench_match!(medium_1K, medium!(), get_text(TXT_1K, medium_suffix()));
bench_match!(medium_32K, medium!(), get_text(TXT_32K, medium_suffix()));
bench_match!(medium_1MB, medium!(), get_text(TXT_1MB, medium_suffix()));

fn hard_suffix() -> String {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ".to_string()
}

macro_rules! hard { () => (r"[ -~]*ABCDEFGHIJKLMNOPQRSTUVWXYZ$") }

bench_match!(hard_32, hard!(), get_text(TXT_32, hard_suffix()));
bench_match!(hard_1K, hard!(), get_text(TXT_1K, hard_suffix()));
bench_match!(hard_32K, hard!(), get_text(TXT_32K, hard_suffix()));
bench_match!(hard_1MB, hard!(), get_text(TXT_1MB, hard_suffix()));

fn reallyhard_suffix() -> String {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ".to_string()
}

macro_rules! reallyhard {
    () => {
        // The point of this being "really" hard is that it should completely
        // thwart any prefix or suffix literal optimizations.
        r"[ -~]*ABCDEFGHIJKLMNOPQRSTUVWXYZ.*"
    }
}

bench_match!(reallyhard_32, reallyhard!(),
             get_text(TXT_32, reallyhard_suffix()));
bench_match!(reallyhard_1K, reallyhard!(),
             get_text(TXT_1K, reallyhard_suffix()));
bench_match!(reallyhard_32K, reallyhard!(),
             get_text(TXT_32K, reallyhard_suffix()));
bench_match!(reallyhard_1MB, reallyhard!(),
             get_text(TXT_1MB, reallyhard_suffix()));

fn reallyhard2_suffix() -> String {
    "Sherlock Holmes".to_string()
}

macro_rules! reallyhard2 { () => (r"\w+\s+Holmes") }

bench_match!(reallyhard2_1K, reallyhard2!(),
             get_text(TXT_1K, reallyhard2_suffix()));
