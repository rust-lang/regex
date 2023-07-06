use lexopt::{Arg, Parser};

use crate::args::{Configurable, Usage};

/// This defines a configuration for overlapping searches.
///
/// Currently, this just controls whether an overlapping search is enabled or
/// not. By default, it's disabled.
#[derive(Debug, Default)]
pub struct Config {
    pub enabled: bool,
}

impl Configurable for Config {
    fn configure(
        &mut self,
        _: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("overlapping") => {
                self.enabled = true;
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[Usage::new(
            "--overlapping",
            "Enable overlapping search.",
            r#"
Enable overlapping search. When this is enabled, the regex matcher will
attempt to report all possible matches. Generally speaking, when one enables
overlapping search, you also want to ensure that '--match-kind all' is given as
well. Otherwise the overlapping search is unlikely to work as one would expect
since any match semantics other than 'all' exclude some subset of matches from
the underlying automaton.

Note that overlapping search is not supported in every regex matcher.
"#,
        )];
        USAGES
    }
}
