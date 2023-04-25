#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Error {
    msg: &'static str,
}

impl Error {
    pub(crate) fn new(msg: &'static str) -> Error {
        Error { msg }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.msg)
    }
}
