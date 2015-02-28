// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by the Rust Project Developers

// Copyright (c) 2014 The Rust Project Developers
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// - Redistributions of source code must retain the above copyright
//   notice, this list of conditions and the following disclaimer.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer in
//   the documentation and/or other materials provided with the
//   distribution.
//
// - Neither the name of "The Computer Language Benchmarks Game" nor
//   the name of "The Computer Language Shootout Benchmarks" nor the
//   names of its contributors may be used to endorse or promote
//   products derived from this software without specific prior
//   written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.

#![feature(old_io, plugin, std_misc)]
#![plugin(regex_macros)]
extern crate regex;

use std::old_io as io;
use std::sync::{Arc, Future};
use regex::{NoExpand, Regex};

#[test]
fn check() {
    static ANSWER: &'static str = "\
agggtaaa|tttaccct 0
[cgt]gggtaaa|tttaccc[acg] 3
a[act]ggtaaa|tttacc[agt]t 9
ag[act]gtaaa|tttac[agt]ct 8
agg[act]taaa|ttta[agt]cct 10
aggg[acg]aaa|ttt[cgt]ccct 3
agggt[cgt]aa|tt[acg]accct 4
agggta[cgt]a|t[acg]taccct 3
agggtaa[cgt]|[acg]ttaccct 5

101745
100000
133640";
    static SEQ: &'static str = include_str!("regexdna-input.txt");
    let got = run(SEQ.to_string()).connect("\n");
    assert_eq!(ANSWER, got);
}

#[allow(dead_code)]
fn main() {
    println!("{}", run(io::stdin().read_to_string().unwrap()).connect("\n"));
}

fn run(mut seq: String) -> Vec<String> {
    let ilen = seq.len();

    seq = regex!(">[^\n]*\n|\n").replace_all(&seq, NoExpand(""));
    let seq_arc = Arc::new(seq.clone()); // copy before it moves
    let clen = seq.len();

    let mut seqlen = Future::spawn(move|| {
        let substs = vec![
            (regex!("B"), "(c|g|t)"),
            (regex!("D"), "(a|g|t)"),
            (regex!("H"), "(a|c|t)"),
            (regex!("K"), "(g|t)"),
            (regex!("M"), "(a|c)"),
            (regex!("N"), "(a|c|g|t)"),
            (regex!("R"), "(a|g)"),
            (regex!("S"), "(c|g)"),
            (regex!("V"), "(a|c|g)"),
            (regex!("W"), "(a|t)"),
            (regex!("Y"), "(c|t)"),
        ];
        let mut seq = seq;
        for (re, replacement) in substs.into_iter() {
            seq = re.replace_all(&seq, NoExpand(replacement));
        }
        seq.len()
    });

    let variants = vec![
        regex!("agggtaaa|tttaccct"),
        regex!("[cgt]gggtaaa|tttaccc[acg]"),
        regex!("a[act]ggtaaa|tttacc[agt]t"),
        regex!("ag[act]gtaaa|tttac[agt]ct"),
        regex!("agg[act]taaa|ttta[agt]cct"),
        regex!("aggg[acg]aaa|ttt[cgt]ccct"),
        regex!("agggt[cgt]aa|tt[acg]accct"),
        regex!("agggta[cgt]a|t[acg]taccct"),
        regex!("agggtaa[cgt]|[acg]ttaccct"),
    ];
    let (mut variant_strs, mut counts) = (vec!(), vec!());
    for variant in variants.into_iter() {
        let seq_arc_copy = seq_arc.clone();
        variant_strs.push(variant.to_string());
        counts.push(Future::spawn(move|| {
            variant.find_iter(&seq_arc_copy).count()
        }));
    }

    let mut olines = Vec::new();
    for (i, variant) in variant_strs.iter().enumerate() {
        olines.push(format!("{} {}", variant, counts[i].get()));
    }
    olines.push("".to_string());
    olines.push(format!("{}", ilen));
    olines.push(format!("{}", clen));
    olines.push(format!("{}", seqlen.get()));
    olines
}
