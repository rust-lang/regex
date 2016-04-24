// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// We don't always use all of the method available to each regex engine because
// of conditional compilation and such.
#![allow(dead_code)]

#[cfg(feature = "re-onig")]
pub mod onig;
#[cfg(feature = "re-pcre1")]
pub mod pcre1;
#[cfg(feature = "re-pcre2")]
pub mod pcre2;
#[cfg(feature = "re-re2")]
pub mod re2;
#[cfg(feature = "re-tcl")]
pub mod tcl;
