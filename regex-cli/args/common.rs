use lexopt::{Arg, Parser};

use crate::args::{Configurable, Usage};

/// This exposes all of the configuration knobs on a regex_automata::Input via
/// CLI flags. The only aspect of regex_automata::Input that this does not
/// cover is the haystack, which should be provided by other means (usually
/// with `Haystack`).
#[derive(Debug, Default)]
pub struct Config {
    pub quiet: bool,
    pub verbose: bool,
    pub no_table: bool,
}

impl Config {
    pub fn table(&self) -> bool {
        !self.no_table
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        _: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Short('q') | Arg::Long("quiet") => {
                self.quiet = true;
            }
            Arg::Long("verbose") => {
                self.verbose = true;
            }
            Arg::Long("no-table") => {
                self.no_table = true;
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "-q, --quiet",
                "Suppress some output.",
                r#"
This is a generic flag that suppresses some (but not all) output. Which output
is suppressed depends on the command. For example, using the -q/--quiet flag
with the 'regex-cli debug' variety of commands will only show the properties of
the objected being printed and will suppress the debug printing of the object
itself.
"#,
            ),
            Usage::new(
                "--verbose",
                "Add more output.",
                r#"
This is a generic flag that expands output beyond the "normal" amount. Which
output is added depends on the command.
"#,
            ),
            Usage::new(
                "--no-table",
                "Omit any table of information from the output.",
                r#"
Many commands in this tool will print a table of property information related
to the task being performed. Passing this flag will suppress that table.
"#,
            ),
        ];
        USAGES
    }
}
