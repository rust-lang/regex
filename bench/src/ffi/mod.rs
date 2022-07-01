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
