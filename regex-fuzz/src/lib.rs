// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate rand;
extern crate regex;
extern crate regex_syntax;
extern crate quickcheck;

use rand::Rng;
use rand::distributions::exponential::Exp1;
use regex_syntax::{Expr, CharClass, ByteClass, ByteRange, ClassRange, Repeater};
use quickcheck::{Arbitrary, Gen, StdGen, Testable, QuickCheck};

/// TODO: implement unicode support
fn randomize_case<G: Rng>(g: &mut G, c: char) -> char {
    match g.gen_range(0, 3) {
        0 => {
            c
        }
        1 => {
            if 'a' <= c && c <= 'z' {
                (c as u8 - 'a' as u8 + 'A' as u8) as char
            } else {
                c
            }
        }
        2 => {
            if 'A' <= c && c <= 'Z' {
                (c as u8 - 'A' as u8 + 'a' as u8) as char
            } else {
                c
            }
        }
        _ => unreachable!(),
    }
}

///Inclusive of both bounds.
fn random_char_in_range<G: Rng>(g: &mut G, low: char, high: char) -> char {
    let (low, high) = (low as u32, high as u32);
    let l0 = low;
    let h0 = ::std::cmp::min(high, 0xD7FF);
    let l1 = ::std::cmp::max(low, 0xE000);
    let h1 = ::std::cmp::min(high, 0x10FFFF);
    let s0 = if h0 < l0 {0} else {h0 - l0 + 1};
    let s1 = if h1 < l1 {0} else {h1 - l1 + 1};
    let x: u32 = if g.gen_range(0, s1 + s0) < s0 {
        g.gen_range(l0, h0 + 1)
    } else {
        g.gen_range(l1, h1 + 1)
    };
    assert!(low <= x && x <= high);
    ::std::char::from_u32(x).unwrap()
}

pub fn sample_language<G: Rng>(g: &mut G, acc: &mut Vec<char>, expr: &Expr) {
    match expr {
        &Expr::Empty => (),
        &Expr::Literal { ref chars, casei, } => {
            if !casei || g.gen() {
                acc.extend(chars.iter());
            } else {
                acc.extend(chars.iter().map(|&c| randomize_case(g, c)))
            }
        }
        &Expr::LiteralBytes { ref bytes, casei, } => {
            if !casei || g.gen() {
                acc.extend(bytes.iter().map(|&x| x as char));
            } else {
                acc.extend(bytes.iter().map(|&x| randomize_case(g, x as char)));
            }
        }
        &Expr::AnyChar => {
            acc.push(g.gen());
        },
        &Expr::AnyCharNoNL => {
            loop {
                let c = g.gen();
                if c != '\n' {
                    acc.push(c);
                    break;
                }
            }
        },
        &Expr::AnyByte => {
            acc.push((g.gen::<u8>() % 128) as char);
        },
        &Expr::AnyByteNoNL => {
            loop {
                let b = g.gen::<u8>() % 128;
                if b != '\n' as u8 {
                    acc.push(b as char);
                    break;
                }
            }
        },
        &Expr::Class(CharClass { ref ranges } ) => {
            let ClassRange { start, end } = {
                ranges[g.gen_range(0, ranges.len())]
            };
            acc.push(random_char_in_range(g, start, end));
        },
        &Expr::ClassBytes(ByteClass { ref ranges }) => {
            let ByteRange { start, end } = {
                ranges[g.gen_range(0, ranges.len())]
            };
            acc.push(g.gen_range(start, end + 1) as char);
        }
        &Expr::Group { ref e, i: _, name: _ } => {
            sample_language(g, acc, e.as_ref());
        },
        &Expr::Repeat { ref e, r, greedy: _, } => {
            let n = match r {
                Repeater::ZeroOrOne => g.gen_range(0, 2),
                Repeater::ZeroOrMore => {let Exp1(x) = g.gen(); x as usize}
                Repeater::OneOrMore => {let Exp1(x) = g.gen(); x as usize + 1}
                Repeater::Range { min: a, max: None} => {
                    let Exp1(x) = g.gen();
                    x as usize + a as usize
                }
                Repeater::Range { min: a, max: Some(b) } =>  {
                    g.gen_range(a as usize, b as usize + 1)
                }
            };
            for _ in 0..n {
                sample_language(g, acc, e);
            }
        },
        &Expr::Concat(ref v) => {
            for e in v.iter() {
                sample_language(g, acc, e);
            }
        },
        &Expr::Alternate(ref v) => {
            let i = g.gen_range(0, v.len());
            sample_language(g, acc, &v[i])
        },
        _ => panic!("zero-width assertions unsupported"),
    }
}

fn remove_zero_width(e: &mut Expr) {
    use regex_syntax::Expr::*;
    match e {
        &mut StartLine | &mut EndLine | &mut StartText |
        &mut EndText | &mut WordBoundary | &mut NotWordBoundary |
        &mut WordBoundaryAscii | &mut NotWordBoundaryAscii => {
            *e = AnyChar;
        }
        &mut Group { ref mut e, .. } => {
            remove_zero_width(e.as_mut());
        }
        &mut Repeat { ref mut e, .. } => {
            remove_zero_width(e.as_mut());
        }
        &mut Concat(ref mut v) => {
            for e in v.iter_mut() {
                remove_zero_width(e);
            }
        }
        &mut Alternate(ref mut v) => {
            for e in v.iter_mut() {
                remove_zero_width(e);
            }
        }
        _ => (),
    }
}

#[derive(Clone, Debug)]
struct ExprWithMatching {
    e: Expr,
    s: String,
}

impl Arbitrary for ExprWithMatching {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let mut e = Arbitrary::arbitrary(g);
        remove_zero_width(&mut e);
        let mut v = Vec::new();
        sample_language(g, &mut v, &e);
        ExprWithMatching {
            e: e,
            s: v.iter().map(|&c| c).collect(),
        }
    }
    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        Box::new(self.e.shrink().map(|e| {
            let mut e = e.clone();
            remove_zero_width(&mut e);
            let mut v = Vec::new();
            let mut g = StdGen::new(rand::thread_rng(), 100);
            sample_language(&mut g, &mut v, &e);
            ExprWithMatching {
                e: e,
                s: v.iter().map(|&c| c).collect(),
            }
        }))
    }
}

fn qc<T: Testable>(t: T) {
    QuickCheck::new()
        .tests(10_000)
        .max_tests(20_000)
        .quickcheck(t);
}

#[test]
fn matching_string_matches() {
    fn prop(ExprWithMatching {e, s}: ExprWithMatching) -> bool {
        use regex::bytes::Regex;
        let re = Regex::new(&e.to_string()).unwrap();
        let caps = re.captures(&s.as_bytes());
        caps.unwrap().pos(0).unwrap().0 == 0
    }
	qc(prop as fn(ExprWithMatching) -> bool);
}
