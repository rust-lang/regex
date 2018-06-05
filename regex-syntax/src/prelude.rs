//! This module provides access to items available in both the standard library
//! and the `alloc` facade crate while concealing the feature-flag based compile
//! time selection between those options.
#![allow(unused_imports)]

macro_rules! multiplex_alloc {
    ($($alloc: path, $std: path),*) => {
        $(
            #[cfg(all(feature = "alloc", not(feature = "std")))]
            pub(crate) use $alloc;
            #[cfg(feature = "std")]
            pub(crate) use $std;
        )*
    };
}

macro_rules! multiplex_core {
    ($($core: path, $std: path),*) => {
        $(
            #[cfg(not(feature = "std"))]
            pub(crate) use $core;
            #[cfg(feature = "std")]
            pub(crate) use $std;
        )*
    };
}

macro_rules! alloc_only {
    ($($alloc: path),*) => {
        $(
            #[cfg(all(feature = "alloc", not(feature = "std")))]
            pub(crate) use $alloc;
        )*
    };
}

multiplex_alloc! {
    alloc::borrow::Borrow, ::std::borrow::Borrow,
    alloc::borrow::ToOwned, ::std::borrow::ToOwned,
    alloc::boxed::Box, ::std::boxed::Box,
    alloc::String, ::std::string::String,
    alloc::string::ToString, ::std::string::ToString,
    alloc::Vec, ::std::vec::Vec
}

multiplex_core! {
    core::fmt, ::std::fmt,
    core::slice, ::std::slice
}

alloc_only! {
    alloc::slice::SliceConcatExt
}

