// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate docopt;
#[macro_use]
extern crate lazy_static;
extern crate libc;
#[cfg(feature = "re-pcre1")]
extern crate libpcre_sys;
extern crate memmap;
#[cfg(feature = "re-onig")]
extern crate onig;
#[cfg(any(
    feature = "re-rust",
    feature = "re-rust-bytes",
    feature = "re-rust-plugin",
  ))]
extern crate regex;
#[cfg(feature = "re-rust")]
extern crate regex_syntax;
extern crate rustc_serialize;

use std::str;

use docopt::Docopt;
use memmap::{Mmap, Protection};

mod ffi;

const USAGE: &'static str = "
Count the number of matches of <pattern> in <file>.

This compiles the pattern once and counts all successive non-overlapping
matches in <file>. <file> is memory mapped. Matching is done as if <file> were
a single string (it is not line oriented).

Since this tool includes compilation of the <pattern>, sufficiently large
haystacks should be used to amortize the cost of compilation. (e.g., >1MB.)

Usage:
    regex-run-one [options] [onig | pcre1 | pcre2 | re2 | rust | rust-bytes | tcl] <pattern> <file>
    regex-run-one [options] (-h | --help)

Options:
    -h, --help   Show this usage message.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_pattern: String,
    arg_file: String,
    cmd_onig: bool,
    cmd_pcre1: bool,
    cmd_pcre2: bool,
    cmd_re2: bool,
    cmd_rust: bool,
    cmd_rust_bytes: bool,
    cmd_tcl: bool,
}

fn main() {
    let args: Args = Docopt::new(USAGE).and_then(|d| d.decode())
                                       .unwrap_or_else(|e| e.exit());

    let mmap = Mmap::open_path(&args.arg_file, Protection::Read).unwrap();
    let haystack = unsafe { str::from_utf8_unchecked(mmap.as_slice()) };

    println!("{}", args.count(&haystack));
}

impl Args {
    fn count(&self, haystack: &str) -> usize {
        let pat = &self.arg_pattern;
        if self.cmd_onig {
            count_onig(pat, haystack)
        } else if self.cmd_pcre1 {
            count_pcre1(pat, haystack)
        } else if self.cmd_pcre2 {
            count_pcre2(pat, haystack)
        } else if self.cmd_re2 {
            count_re2(pat, haystack)
        } else if self.cmd_rust {
            count_rust(pat, haystack)
        } else if self.cmd_rust_bytes {
            count_rust_bytes(pat, haystack)
        } else if self.cmd_tcl {
            count_tcl(pat, haystack)
        } else {
            panic!("unreachable")
        }
    }
}

macro_rules! nada {
    ($feature:expr, $name:ident) => {
        #[cfg(not(feature = $feature))]
        fn $name(_pat: &str, _haystack: &str) -> usize {
            panic!("Support not enabled. Re-compile with '--features {}' \
                    to enable.", $feature)
        }
    }
}

nada!("re-onig", count_onig);
#[cfg(feature = "re-onig")]
fn count_onig(pat: &str, haystack: &str) -> usize {
    use ffi::onig::Regex;
    Regex::new(pat).unwrap().find_iter(haystack).count()
}

nada!("re-pcre1", count_pcre1);
#[cfg(feature = "re-pcre1")]
fn count_pcre1(pat: &str, haystack: &str) -> usize {
    use ffi::pcre1::Regex;
    Regex::new(pat).unwrap().find_iter(haystack).count()
}

nada!("re-pcre2", count_pcre2);
#[cfg(feature = "re-pcre2")]
fn count_pcre2(pat: &str, haystack: &str) -> usize {
    use ffi::pcre2::Regex;
    Regex::new(pat).unwrap().find_iter(haystack).count()
}

nada!("re-re2", count_re2);
#[cfg(feature = "re-re2")]
fn count_re2(pat: &str, haystack: &str) -> usize {
    use ffi::re2::Regex;
    Regex::new(pat).unwrap().find_iter(haystack).count()
}

nada!("re-rust", count_rust);
#[cfg(feature = "re-rust")]
fn count_rust(pat: &str, haystack: &str) -> usize {
    use regex::Regex;
    Regex::new(pat).unwrap().find_iter(haystack).count()
}

nada!("re-rust-bytes", count_rust_bytes);
#[cfg(feature = "re-rust-bytes")]
fn count_rust_bytes(pat: &str, haystack: &str) -> usize {
    use regex::bytes::Regex;
    Regex::new(pat).unwrap().find_iter(haystack.as_bytes()).count()
}

nada!("re-tcl", count_tcl);
#[cfg(feature = "re-tcl")]
fn count_tcl(pat: &str, haystack: &str) -> usize {
    use ffi::tcl::{Regex, Text};
    Regex::new(pat).unwrap().find_iter(&Text::new(haystack.to_owned())).count()
}
