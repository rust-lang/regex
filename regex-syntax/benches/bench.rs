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

extern crate regex_syntax;
extern crate test;

use regex_syntax::Expr;
use test::Bencher;

#[bench]
fn parse_simple1(b: &mut Bencher) {
    b.iter(|| {
        let re = r"^bc(d|e)*$";
        Expr::parse(re).unwrap()
    });
}

#[bench]
fn parse_simple2(b: &mut Bencher) {
    b.iter(|| {
        let re = r"'[a-zA-Z_][a-zA-Z0-9_]*(')\b";
        Expr::parse(re).unwrap()
    });
}

#[bench]
fn parse_small1(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\p{L}|\p{N}|\s|.|\d";
        Expr::parse(re).unwrap()
    });
}

#[bench]
fn parse_medium1(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\pL\p{Greek}\p{Hiragana}\p{Alphabetic}\p{Hebrew}\p{Arabic}";
        Expr::parse(re).unwrap()
    });
}

#[bench]
fn parse_medium2(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\s\S\w\W\d\D";
        Expr::parse(re).unwrap()
    });
}

#[bench]
fn parse_huge(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\p{L}{100}";
        Expr::parse(re).unwrap()
    });
}
