// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// contributed by BurntSushi

extern crate regex;

use std::io::{self, Read};
use std::sync::Arc;
use std::thread;

macro_rules! regex { ($re:expr) => { ::regex::Regex::new($re).unwrap() } }

fn main() {
    let mut seq = String::with_capacity(50 * (1 << 20));
    io::stdin().read_to_string(&mut seq).unwrap();
    let ilen = seq.len();

    seq = regex!(">[^\n]*\n|\n").replace_all(&seq, "");
    let clen = seq.len();
    let seq_arc = Arc::new(seq.clone());

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
    let mut count_promises = vec![];
    for i in 0..3 {
        let seq = seq_arc.clone();
        let res = variants[i * 3..i * 3 + 3].to_vec();
        count_promises.push(thread::spawn(move || {
            res.into_iter()
               .map(|re| (re.to_string(), re.find_iter(&seq).count()))
               .collect::<Vec<_>>()
        }));
    }

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
        seq = re.replace_all(&seq, replacement);
    }

    for promise in count_promises {
        for (re, count) in promise.join().unwrap() {
            println!("{} {}", re, count);
        }
    }
    println!("\n{}\n{}\n{}", ilen, clen, seq.len());
}
