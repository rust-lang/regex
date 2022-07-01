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
