pub use self::imp::*;

#[cfg(target_arch = "x86_64")]
mod imp;

#[cfg(not(target_arch = "x86_64"))]
#[path = "fallback.rs"]
mod imp;
