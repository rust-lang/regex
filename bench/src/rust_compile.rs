// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use regex_syntax::Expr;
use test::Bencher;

use regex::internal::Compiler;

#[bench]
fn compile_simple(b: &mut Bencher) {
    b.iter(|| {
        let re = Expr::parse(r"^bc(d|e)*$").unwrap();
        Compiler::new().compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_simple_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = Expr::parse(r"^bc(d|e)*$").unwrap();
        Compiler::new().bytes(true).compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_simple_full(b: &mut Bencher) {
    b.iter(|| {
        regex!(r"^bc(d|e)*$")
    });
}

#[bench]
fn compile_small(b: &mut Bencher) {
    b.iter(|| {
        let re = Expr::parse(r"\p{L}|\p{N}|\s|.|\d").unwrap();
        Compiler::new().compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_small_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = Expr::parse(r"\p{L}|\p{N}|\s|.|\d").unwrap();
        Compiler::new().bytes(true).compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_small_full(b: &mut Bencher) {
    b.iter(|| {
        regex!(r"\p{L}|\p{N}|\s|.|\d")
    });
}

#[bench]
fn compile_huge(b: &mut Bencher) {
    b.iter(|| {
        let re = Expr::parse(r"\p{L}{100}").unwrap();
        Compiler::new().compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_huge_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = Expr::parse(r"\p{L}{100}").unwrap();
        Compiler::new().bytes(true).compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_huge_full(b: &mut Bencher) {
    b.iter(|| {
        regex!(r"\p{L}{100}")
    });
}
