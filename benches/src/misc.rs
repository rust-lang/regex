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

use rand::{Rng, thread_rng};
use test::Bencher;

use Regex;

#[cfg(not(feature = "re-rust-bytes"))]
macro_rules! text { ($text:expr) => { $text } }
#[cfg(feature = "re-rust-bytes")]
macro_rules! text { ($text:expr) => { $text.as_bytes() } }

macro_rules! bench_match {
    ($name:ident, $re:expr, $text:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            #![allow(unused_mut)]
            use std::sync::Mutex;

            lazy_static! {
                static ref RE: Mutex<Regex> = Mutex::new($re);
                static ref TEXT: String = $text;
            };
            let mut re = RE.lock().unwrap();
            b.bytes = TEXT.len() as u64;
            b.iter(|| {
                if !re.is_match(text!(&TEXT)) {
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
            #![allow(unused_mut)]
            use std::sync::Mutex;

            lazy_static! {
                static ref RE: Mutex<Regex> = Mutex::new($re);
                static ref TEXT: String = $text;
            };
            let mut re = RE.lock().unwrap();
            b.bytes = TEXT.len() as u64;
            b.iter(|| {
                if re.is_match(text!(&TEXT)) {
                    panic!("match not expected");
                }
            });
        }
    }
}

#[cfg(not(feature = "re-onig"))]
#[cfg(not(feature = "re-pcre"))]
bench_match!(no_exponential, {
    let re = format!(
        "{}{}",
        repeat("a?").take(100).collect::<String>(),
        repeat("a").take(100).collect::<String>());
    // We don't use the macro here since we're dynamically building the regex.
    Regex::new(&re).unwrap()
}, repeat("a").take(100).collect());

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

#[cfg(not(feature = "re-rust-bytes"))]
bench_match!(match_class_unicode, regex!(r"\p{L}"), {
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

fn easy0() -> Regex {
    regex!("ABCDEFGHIJKLMNOPQRSTUVWXYZ$")
}

bench_nomatch!(easy0_32, easy0(), gen_text(32));
bench_nomatch!(easy0_1K, easy0(), gen_text(1<<10));
bench_nomatch!(easy0_32K, easy0(), gen_text(32<<10));
bench_nomatch!(easy0_1MB, easy0(), gen_text(1<<20));

fn easy1() -> Regex {
    regex!("A[AB]B[BC]C[CD]D[DE]E[EF]F[FG]G[GH]H[HI]I[IJ]J$")
}

bench_nomatch!(easy1_32, easy1(), gen_text(32));
bench_nomatch!(easy1_1K, easy1(), gen_text(1<<10));
bench_nomatch!(easy1_32K, easy1(), gen_text(32<<10));
bench_nomatch!(easy1_1MB, easy1(), gen_text(1<<20));

fn medium() -> Regex {
    regex!("[XYZ]ABCDEFGHIJKLMNOPQRSTUVWXYZ$")
}

bench_nomatch!(medium_32, medium(), gen_text(32));
bench_nomatch!(medium_1K, medium(), gen_text(1<<10));
bench_nomatch!(medium_32K, medium(), gen_text(32<<10));
bench_nomatch!(medium_1MB, medium(), gen_text(1<<20));

fn hard() -> Regex {
    regex!("[ -~]*ABCDEFGHIJKLMNOPQRSTUVWXYZ$")
}

bench_nomatch!(hard_32, hard(), gen_text(32));
bench_nomatch!(hard_1K, hard(), gen_text(1<<10));
bench_nomatch!(hard_32K, hard(), gen_text(32<<10));
bench_nomatch!(hard_1MB, hard(), gen_text(1<<20));

#[cfg(feature = "re-rust")]
#[bench]
fn replace_all(b: &mut Bencher) {
    let re = regex!("[cjrw]");
    let text = "abcdefghijklmnopqrstuvwxyz";
    b.iter(|| re.replace_all(text, ""));
}
