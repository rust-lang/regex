/*!
This crate provides an "expert" API for executing regular expressions using
finite automata.

**WARNING**: This `0.2` release of `regex-automata` was published
before it was ready to unblock work elsewhere that needed some
of the new APIs in this release. At the time of writing, it is
strongly preferred that you continue using the
[`regex-automata 0.1`](https://docs.rs/regex-automata/0.1/regex_automata/)
release. Since this release represents an unfinished state, please do not
create issues for this release unless it's for a critical bug.
*/

// We are no_std.
#![no_std]
// All APIs need docs!
#![deny(missing_docs)]
// Some intra-doc links are broken when certain features are disabled, so we
// only bleat about it when most (all?) features are enabled. But when we do,
// we block the build. Links need to work.
#![cfg_attr(
    all(
        feature = "std",
        feature = "nfa",
        feature = "dfa",
        feature = "hybrid"
    ),
    deny(rustdoc::broken_intra_doc_links)
)]
// Broken rustdoc links are very easy to come by when you start disabling
// features. Namely, features tend to change imports, and imports change what's
// available to link to.
//
// Basically, we just don't support rustdoc for anything other than the maximal
// feature configuration. Other configurations will work, they just won't be
// perfect.
//
// So here, we specifically allow them so we don't even get warned about them.
#![cfg_attr(
    not(all(
        feature = "std",
        feature = "nfa",
        feature = "dfa",
        feature = "hybrid"
    )),
    allow(rustdoc::broken_intra_doc_links)
)]
// Kinda similar, but eliminating all of the dead code and unused import
// warnings for every feature combo is a fool's errand. Instead, we just
// suppress those, but still let them through in a common configuration when we
// build most of everything.
//
// This does actually suggest that when features are disabled, we are actually
// compiling more code than we need to be. And this is perhaps not so great
// because disabling features is usually done in order to reduce compile times
// by reducing the amount of code one compiles... However, usually, most of the
// time this dead code is a relatively small amount from the 'util' module.
// But... I confess... There isn't a ton of visibility on this.
//
// I'm happy to try to address this in a different way, but "let's annotate
// every function in 'util' with some non-local combination of features" just
// cannot be the way forward.
#![cfg_attr(
    not(all(
        feature = "std",
        feature = "nfa",
        feature = "dfa",
        feature = "hybrid",
        feature = "perf-literal-substring",
        feature = "perf-literal-multisubstring",
    )),
    allow(dead_code, unused_imports, unused_variables)
)]
// We generally want all types to impl Debug.
#![warn(missing_debug_implementations)]
// No clue why this thing is still unstable because it's pretty amazing. This
// adds Cargo feature annotations to items in the rustdoc output. Which is
// sadly hugely beneficial for this crate due to the number of features.
#![cfg_attr(docsrs, feature(doc_auto_cfg))]

// I have literally never tested this crate on 16-bit, so it is quite
// suspicious to advertise support for it. But... the regex crate, at time
// of writing, at least claims to support it by not doing any conditional
// compilation based on the target pointer width. So I guess I remain
// consistent with that here.
//
// If you are here because you're on a 16-bit system and you were somehow using
// the regex crate previously, please file an issue. Please be prepared to
// provide some kind of reproduction or carve out some path to getting 16-bit
// working in CI. (Via qemu?)
#[cfg(not(any(
    target_pointer_width = "16",
    target_pointer_width = "32",
    target_pointer_width = "64"
)))]
compile_error!("not supported on non-{16,32,64}, please file an issue");

#[cfg(any(test, feature = "std"))]
extern crate std;

#[cfg(feature = "alloc")]
extern crate alloc;

#[doc(inline)]
pub use crate::util::primitives::PatternID;
pub use crate::util::search::*;

#[macro_use]
mod macros;

#[cfg(any(feature = "dfa-search", feature = "dfa-onepass"))]
pub mod dfa;
#[cfg(feature = "hybrid")]
pub mod hybrid;
#[cfg(feature = "meta")]
pub mod meta;
#[cfg(feature = "nfa-thompson")]
pub mod nfa;
pub mod util;
