// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate pkg_config;

use std::env;
use std::process;

use pkg_config::Config;

macro_rules! we {
    ($($tt:tt)*) => {{
        use std::io::Write;
        writeln!(&mut ::std::io::stderr(), $($tt),*).unwrap();
    }}
}

fn main() {
    let wants_pcre2 = env::var("CARGO_FEATURE_RE_PCRE2").is_ok();
    let has_pcre2 =
        Config::new().atleast_version("10.21").find("libpcre2-8").is_ok();
    if wants_pcre2 && !has_pcre2 {
        we!("pcre2 cannot be found by pkg-config");
        process::exit(1);
    }
}
