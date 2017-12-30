// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate cc;
extern crate pkg_config;

use std::env;

fn main() {
    if env::var("CARGO_FEATURE_RE_PCRE2").is_ok() {
        pkg_config::probe_library("libpcre2-8").unwrap();
    }
    if env::var("CARGO_FEATURE_RE_RE2").is_ok() {
        // RE2 is a C++ library, so we need to compile our shim layer.
        cc::Build::new()
            .cpp(true)
            .file("src/ffi/re2.cpp")
            .compile("libcre2.a");
        // It's important this comes after compiling the shim, which results
        // in the correct order of arguments given to the linker.
        pkg_config::probe_library("re2").unwrap();
    }
    if env::var("CARGO_FEATURE_RE_TCL").is_ok() {
        pkg_config::probe_library("tcl").unwrap();
    }
}
