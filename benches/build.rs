// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate gcc;
extern crate pkg_config;

use std::env;
use std::process;

macro_rules! we {
    ($($tt:tt)*) => {{
        use std::io::Write;
        writeln!(&mut ::std::io::stderr(), $($tt),*).unwrap();
    }}
}

fn main() {
    // We only need to look for PCRE2 and RE2 because we roll the FFI bindings
    // for those libraries ourselves from scratch. For PCRE1 and Oniguruma, we
    // rely on other crates that do something similar to the dance below for
    // us.

    let wants_pcre2 = env::var("CARGO_FEATURE_RE_PCRE2").is_ok();
    let has_pcre2 = pkg_config::Config::new().find("libpcre2-8").is_ok();
    if wants_pcre2 && !has_pcre2 {
        we!("pcre2 cannot be found by pkg-config");
        process::exit(1);
    }

    let wants_re2 = env::var("CARGO_FEATURE_RE_RE2").is_ok();
    let has_re2 = pkg_config::Config::new().find("re2").is_ok();
    if wants_re2 {
        if !has_re2 {
            we!("re2 cannot be found by pkg-config");
            process::exit(1);
        }
        gcc::Config::new()
            .cpp(true)
            .flag("-std=c++11")
            .file("src/ffi/re2.cpp")
            .compile("libcre2.a");
        println!("cargo:rustc-link-lib=re2");
    }

    let wants_tcl = env::var("CARGO_FEATURE_RE_TCL").is_ok();
    let has_tcl = pkg_config::Config::new().find("tcl").is_ok();
    if wants_tcl && !has_tcl {
        we!("tcl cannot be found by pkg-config");
        process::exit(1);
    }
}
