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

use regex::internal::ProgramBuilder;

#[bench]
fn compile_simple(b: &mut Bencher) {
    b.iter(|| {
        let re = r"^bc(d|e)*$";
        ProgramBuilder::new(&re).compile().unwrap()
    });
}

#[bench]
fn compile_simple_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = r"^bc(d|e)*$";
        ProgramBuilder::new(&re).bytes(true).compile().unwrap()
    });
}

#[bench]
fn compile_small(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\p{L}|\p{N}|\s|.|\d";
        ProgramBuilder::new(&re).compile().unwrap()
    });
}

#[bench]
fn compile_small_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\p{L}|\p{N}|\s|.|\d";
        ProgramBuilder::new(&re).bytes(true).compile().unwrap()
    });
}

#[bench]
fn compile_huge(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\p{L}{100}";
        ProgramBuilder::new(&re).compile().unwrap()
    });
}

#[bench]
fn compile_huge_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\p{L}{100}";
        ProgramBuilder::new(&re).bytes(true).compile().unwrap()
    });
}
