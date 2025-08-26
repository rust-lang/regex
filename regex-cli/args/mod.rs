use std::{
    fmt::{Debug, Display, Write},
    str::FromStr,
};

use {
    anyhow::Context,
    lexopt::{Arg, Parser, ValueExt},
};

pub mod api;
pub mod backtrack;
pub mod common;
pub mod dfa;
pub mod flags;
pub mod haystack;
pub mod hybrid;
pub mod input;
pub mod lite;
pub mod meta;
pub mod onepass;
pub mod overlapping;
pub mod patterns;
pub mod pikevm;
pub mod syntax;
pub mod thompson;

pub trait Configurable: Debug {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool>;

    fn usage(&self) -> &[Usage];
}

pub fn configure(
    p: &mut Parser,
    usage: &str,
    targets: &mut [&mut dyn Configurable],
) -> anyhow::Result<()> {
    while let Some(arg) = p.next()? {
        match arg {
            Arg::Short('h') | Arg::Long("help") => {
                let mut usages = vec![];
                for t in targets.iter() {
                    usages.extend_from_slice(t.usage());
                }
                usages.sort_by_key(|u| {
                    u.format
                        .split_once(", ")
                        .map(|(_, long)| long)
                        .unwrap_or(u.format)
                });
                let options = if arg == Arg::Short('h') {
                    Usage::short(&usages)
                } else {
                    Usage::long(&usages)
                };
                let usage = usage.replace("%options%", &options);
                anyhow::bail!("{}", usage.trim());
            }
            _ => {}
        }
        // We do this little dance to disentangle the lifetime of 'p' from the
        // lifetime on 'arg'. The cost is that we have to clone all long flag
        // names to give it a place to live that isn't tied to 'p'. Annoying,
        // but not the end of the world.
        let long_flag: Option<String> = match arg {
            Arg::Long(name) => Some(name.to_string()),
            _ => None,
        };
        let mut arg = match long_flag {
            Some(ref flag) => Arg::Long(flag),
            None => match arg {
                Arg::Short(c) => Arg::Short(c),
                Arg::Long(_) => unreachable!(),
                Arg::Value(value) => Arg::Value(value),
            },
        };
        // OK, now ask all of our targets whether they want this argument.
        let mut recognized = false;
        for t in targets.iter_mut() {
            if t.configure(p, &mut arg)? {
                recognized = true;
                break;
            }
        }
        if !recognized {
            return Err(arg.unexpected().into());
        }
    }
    Ok(())
}

/*
pub struct AdHoc<'a> {
    usage: Usage,
    configure:
        Box<dyn FnMut(&mut Parser, &mut Arg) -> anyhow::Result<bool> + 'a>,
}

impl<'a> AdHoc<'a> {
    pub fn new(
        usage: Usage,
        configure: impl FnMut(&mut Parser, &mut Arg) -> anyhow::Result<bool> + 'a,
    ) -> AdHoc<'a> {
        AdHoc { usage, configure: Box::new(configure) }
    }
}

impl<'a> Configurable for AdHoc<'a> {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        (self.configure)(p, arg)
    }

    fn usage(&self) -> &[Usage] {
        std::slice::from_ref(&self.usage)
    }
}

impl<'a> Debug for AdHoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("AdHoc")
            .field("usage", &self.usage)
            .field("configure", &"FnMut(..)")
            .finish()
    }
}
*/

/// Parses the argument from the given parser as a command name, and returns
/// it. If the next arg isn't a simple value then this returns an error.
///
/// This also handles the case where -h/--help is given, in which case, the
/// given usage information is converted into an error and printed.
pub fn next_as_command(usage: &str, p: &mut Parser) -> anyhow::Result<String> {
    let usage = usage.trim();
    let arg = match p.next()? {
        Some(arg) => arg,
        None => anyhow::bail!("{usage}"),
    };
    let cmd = match arg {
        Arg::Value(cmd) => cmd.string()?,
        Arg::Short('h') | Arg::Long("help") => anyhow::bail!("{usage}"),
        arg => return Err(arg.unexpected().into()),
    };
    Ok(cmd)
}

/// Parses the next 'p.value()' into 'T'. Any error messages will include the
/// given flag name in them.
pub fn parse<T>(p: &mut Parser, flag_name: &'static str) -> anyhow::Result<T>
where
    T: FromStr,
    <T as FromStr>::Err: Display + Debug + Send + Sync + 'static,
{
    // This is written somewhat awkwardly and the type signature is also pretty
    // funky primarily because of the following two things: 1) the 'FromStr'
    // impls in this crate just use 'anyhow::Error' for their error type and 2)
    // 'anyhow::Error' does not impl 'std::error::Error'.
    let osv = p.value().context(flag_name)?;
    let strv = match osv.to_str() {
        Some(strv) => strv,
        None => {
            let err = lexopt::Error::NonUnicodeValue(osv.into());
            return Err(anyhow::Error::from(err).context(flag_name));
        }
    };
    let parsed = match strv.parse() {
        Err(err) => return Err(anyhow::Error::msg(err).context(flag_name)),
        Ok(parsed) => parsed,
    };
    Ok(parsed)
}

/// Like `parse`, but permits the string value "none" to indicate absent. This
/// is useful for parsing things like limits, where "no limit" is a legal
/// value. But it can be used for anything.
pub fn parse_maybe<T>(
    p: &mut Parser,
    flag_name: &'static str,
) -> anyhow::Result<Option<T>>
where
    T: FromStr,
    <T as FromStr>::Err: Display + Debug + Send + Sync + 'static,
{
    // This is written somewhat awkwardly and the type signature is also pretty
    // funky primarily because of the following two things: 1) the 'FromStr'
    // impls in this crate just use 'anyhow::Error' for their error type and 2)
    // 'anyhow::Error' does not impl 'std::error::Error'.
    let osv = p.value().context(flag_name)?;
    let strv = match osv.to_str() {
        Some(strv) => strv,
        None => {
            let err = lexopt::Error::NonUnicodeValue(osv.into());
            return Err(anyhow::Error::from(err).context(flag_name));
        }
    };
    if strv == "none" {
        return Ok(None);
    }
    let parsed = match strv.parse() {
        Err(err) => return Err(anyhow::Error::msg(err).context(flag_name)),
        Ok(parsed) => parsed,
    };
    Ok(Some(parsed))
}

/// A type for expressing the documentation of a flag.
///
/// The `Usage::short` and `Usage::long` functions take a slice of usages and
/// format them into a human readable display. It does simple word wrapping and
/// column alignment for you.
#[derive(Clone, Copy, Debug)]
pub struct Usage {
    /// The format of the flag, for example, `-k, --match-kind <kind>`.
    pub format: &'static str,
    /// A very short description of the flag. Should fit on one line along with
    /// the format.
    pub short: &'static str,
    /// A longer form description of the flag. May be multiple paragraphs long
    /// (but doesn't have to be).
    pub long: &'static str,
}

impl Usage {
    /// Create a new usage from the given components.
    pub const fn new(
        format: &'static str,
        short: &'static str,
        long: &'static str,
    ) -> Usage {
        Usage { format, short, long }
    }

    /// Format a two column table from the given usages, where the first
    /// column is the format and the second column is the short description.
    pub fn short(usages: &[Usage]) -> String {
        const MIN_SPACE: usize = 2;

        let mut result = String::new();
        let max_len = match usages.iter().map(|u| u.format.len()).max() {
            None => return result,
            Some(len) => len,
        };
        for usage in usages.iter() {
            let padlen = MIN_SPACE + (max_len - usage.format.len());
            let padding = " ".repeat(padlen);
            writeln!(result, "    {}{}{}", usage.format, padding, usage.short)
                .unwrap();
        }
        result
    }

    /// Print the format of each usage and its long description below the
    /// format. This also does appropriate indentation with the assumption that
    /// it is in an OPTIONS section of a bigger usage message.
    pub fn long(usages: &[Usage]) -> String {
        let wrap_opts = textwrap::Options::new(79)
            .initial_indent("        ")
            .subsequent_indent("        ");
        let mut result = String::new();
        for (i, usage) in usages.iter().enumerate() {
            if i > 0 {
                writeln!(result, "").unwrap();
            }
            writeln!(result, "    {}", usage.format).unwrap();
            for (i, paragraph) in usage.long.trim().split("\n\n").enumerate() {
                if i > 0 {
                    result.push('\n');
                }
                let flattened = paragraph.replace("\n", " ");
                for line in textwrap::wrap(&flattened, &wrap_opts) {
                    result.push_str(&line);
                    result.push('\n');
                }
            }
        }
        result
    }
}
