/*!
TODO
*/

#![no_std]
// I'm not ideologically opposed to allowing non-safe code in this crate, but
// IMO it needs really excellent justification. One likely place where this
// could show up is if and when we support a non-std alloc mode. In that case,
// we need some way to synchronize access to a PikeVM cache. That in turn will
// likely require rolling our own primitive spin-lock or similar structure.
#![forbid(unsafe_code)]
// #![deny(missing_docs, rustdoc::broken_intra_doc_links)]
#![warn(missing_debug_implementations)]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]

#[cfg(not(feature = "std"))]
compile_error!("'std' is currently a required feature, please file an issue");

#[cfg(not(any(target_pointer_width = "32", target_pointer_width = "64")))]
compile_error!("not supported on non-{32,64}, please file an issue");

extern crate alloc;
#[cfg(any(test, feature = "std"))]
extern crate std;

pub use self::{error::Error, hir::escape, string::*};

mod error;
mod hir;
mod int;
mod interpolate;
mod nfa;
mod pikevm;
mod pool;
mod string;
mod utf8;
