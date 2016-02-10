extern crate regex;

use std::io::{self, Read};

macro_rules! regex {
    ($re:expr) => {{
        use regex::internal::MatchEngine;
        ::regex::Regex::with_engine(
            $re, MatchEngine::Automatic, 10 * (1<<20), false).unwrap()
    }}
}

fn main() {
    let mut seq = String::with_capacity(50 * (1 << 20));
    io::stdin().read_to_string(&mut seq).unwrap();
    let ilen = seq.len();

    seq = regex!(">[^\n]*\n|\n").replace_all(&seq, "");
    println!("original: {}, replaced: {}", ilen, seq.len());
}
