use std::path::PathBuf;

use {
    anyhow::Context,
    bstr::{BStr, BString, ByteSlice, ByteVec},
    lexopt::{Arg, Parser, ValueExt},
};

use crate::args::{Configurable, Usage};

/// A configuration object for reading a single haystack from the command line.
///
/// This supports reading either an inline haystack specified via the
/// `-y/--haystack` flag, or via a positional argument pointing to a file path.
///
/// This supports reading exactly one haystack. If more than one are provided,
/// then an error is returned at configuration time. If none are provided, then
/// an error is returned when one attempts to retrieve the haystack.
#[derive(Debug, Default)]
pub struct Config {
    kind: Option<Kind>,
}

impl Config {
    /// Returns the haystack contents in this configuration.
    ///
    /// If the haystack was specified via a file path, then this returns the
    /// entire contents of the file on to the heap.
    pub fn get(&self) -> anyhow::Result<BString> {
        match self.kind {
            Some(Kind::Inline(ref haystack)) => Ok(haystack.clone()),
            Some(Kind::Path(ref path)) => {
                let contents = std::fs::read(&path).with_context(|| {
                    anyhow::anyhow!("failed to read {}", path.display())
                })?;
                Ok(BString::from(contents))
            }
            None => anyhow::bail!(
                "haystack is required via the -y/--haystack flag \
                 or via a positional argument",
            ),
        }
    }

    /// If the haystack is a file, then memory map and pass the contents of the
    /// file to the given closure. Otherwise, if it's an inline literal, then
    /// pass it to the closure as-is.
    pub fn with<T>(
        &self,
        mut f: impl FnMut(&BStr) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        match self.kind {
            Some(Kind::Inline(ref haystack)) => f(haystack.as_bstr()),
            Some(Kind::Path(ref path)) => {
                let file = std::fs::File::open(path).with_context(|| {
                    format!("failed to open {}", path.display())
                })?;
                // SAFETY: We assume this is OK to do since we assume that our
                // search input is immutable. We specifically never try to
                // mutate the bytes from the file or treat them as anything
                // other than a slice of bytes.
                let mmap = unsafe {
                    memmap2::Mmap::map(&file).with_context(|| {
                        format!("failed to mmap {}", path.display())
                    })?
                };
                f(<&BStr>::from(&*mmap))
            }
            None => anyhow::bail!(
                "haystack is required via the -y/--haystack flag \
                 or via a positional argument",
            ),
        }
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Short('y') | Arg::Long("haystack") => {
                anyhow::ensure!(
                    self.kind.is_none(),
                    "only one haystack is allowed",
                );
                let hay = p.value().context("-y/--haystack needs a value")?;
                let hay = hay
                    .string()
                    .context("-y/--haystack must be valid UTF-8")?;
                let hay = Vec::unescape_bytes(&hay);
                self.kind = Some(Kind::Inline(BString::from(hay)));
            }
            Arg::Value(ref mut v) => {
                anyhow::ensure!(
                    self.kind.is_none(),
                    "only one haystack is allowed",
                );
                let path = PathBuf::from(std::mem::take(v));
                self.kind = Some(Kind::Path(path));
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[Usage::new(
            "-y, --haystack <haystack>",
            "Provide an inline haystack on the command line.",
            r#"
This flag provides an inline haystack on the command line. That is, the value
of this flag is *not* a file path, but the haystack contents itself. This is
convenient for small regex searches because it lets one skip creating a file or
other shenanigans.

The haystack contents must be valid UTF-8, but it support escape sequences. So
for example, "-y 'a\xFF\t'" corresponds to the byte sequence 0x61 0xFF 0x09.

Note that exactly one haystack is permitted. The haystack can either be
specified inline with this flag, or can be provided as a file path via a
positional argument.
"#,
        )];
        USAGES
    }
}

/// The kind of haystack specified on the command line.
///
/// We don't read the file path contents at arg parsing time so that we can
/// be a little more flexible. For example, by providing a way to memory map
/// the haystack contents.
#[derive(Debug)]
enum Kind {
    Inline(BString),
    Path(PathBuf),
}
