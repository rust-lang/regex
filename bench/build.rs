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
use std::process;

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

    if env::var("CARGO_FEATURE_RE_DPHOBOS_DMD").is_ok() {
        process::Command::new("dmd")
                    .arg("--version")
                    .stdout(process::Stdio::null())
                    .stderr(process::Stdio::null())
                    .spawn()
                    .unwrap();

        let out_dir = env::var("OUT_DIR").unwrap();
        let out_file = &format!("-of={}/libdphobos-dmd.a", out_dir);

        let res = process::Command::new("dmd")
            .arg("-w")
            .arg("-lib")
            .arg("-O")
            .arg("-release")
            .arg("-inline")
            .arg("src/ffi/d_phobos.d")
            .arg(out_file)
            .output()
            .expect("unable to compile dphobos-regex (dmd)");

        if !res.status.success() {
            println!("{}", String::from_utf8_lossy(&res.stderr));
        }
        assert!(res.status.success());

        println!("cargo:rustc-link-search=native={}", out_dir);
        println!("cargo:rustc-link-lib=dphobos-dmd");
        println!("cargo:rustc-link-lib=phobos2");
    }

    if env::var("CARGO_FEATURE_RE_DPHOBOS_LDC").is_ok() {
        process::Command::new("ldc")
                    .arg("--version")
                    .stdout(process::Stdio::null())
                    .stderr(process::Stdio::null())
                    .spawn()
                    .unwrap();

        let out_dir = env::var("OUT_DIR").unwrap();
        let out_file = &format!("-of={}/libdphobos-ldc.a", out_dir);

        let res = process::Command::new("ldc")
            .arg("-w")
            .arg("-lib")
            .arg("-O3")
            .arg("-release")
            .arg("-mcpu=native")
            .arg("src/ffi/d_phobos.d")
            .arg(out_file)
            .output()
            .expect("unable to compile dphobos-regex (ldc)");

        if !res.status.success() {
            println!("{}", String::from_utf8_lossy(&res.stderr));
        }
        assert!(res.status.success());

        println!("cargo:rustc-link-search=native={}", out_dir);
        println!("cargo:rustc-link-lib=dphobos-ldc");
        println!("cargo:rustc-link-lib=druntime-ldc");
        println!("cargo:rustc-link-lib=phobos2-ldc");
    }
}
