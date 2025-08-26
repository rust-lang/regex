use lexopt::{Arg, Parser};

use crate::args::{self, Configurable, Usage};

mod capture;
mod half;
mod r#match;
mod which;

pub fn run(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search.

The sub-command determines what kind of search to execute. The kind of the
search to execute also determines which regex engines are available. For
example, a lazy DFA cannot report the match positions of capture groups, so
there is no 'regex-cli find capture hybrid' command.

USAGE:
    regex-cli find <command>

COMMANDS:
    capture   Search for regexes with capture groups.
    half      Search for half matches.
    match     Search for full matches.
    which     Search for which patterns match in a set.
";
    let cmd = args::next_as_command(USAGE, p)?;
    match &*cmd {
        "capture" => capture::run(p),
        "half" => half::run(p),
        "match" => r#match::run(p),
        "which" => which::run(p),
        unk => anyhow::bail!("unrecognized command '{unk}'"),
    }
}

#[derive(Debug, Default)]
struct Config {
    count: bool,
    repeat: Option<u32>,
}

impl Config {
    fn repeat(&self) -> u32 {
        self.repeat.unwrap_or(1)
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Short('c') | Arg::Long("count") => {
                self.count = true;
            }
            Arg::Long("repeat") => {
                self.repeat = Some(args::parse(p, "--repeat")?);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &[Usage] = &[
            Usage::new(
                "-c, --count",
                "Show a count of all matches.",
                r#"
Prints a count of all matches instead of printing the matches themselves. For
the 'capture' command, this prints the number of times each group matched. For
the 'which' command, this just prints whether each pattern matched or not.
"#,
            ),
            Usage::new(
                "--repeat",
                "Repeat the search this many times.",
                r#"
Repeat the search this many times. By default, this is set to 1. This is useful
when you want the search time to dominate the runtime of the program, or if the
search is otherwise too short/fast to measure reliably.

Note that this will print the matches repeatedly by default as well. For this
reason, it's usually best to use this option in combination with -c/--count.
"#,
            ),
        ];
        USAGES
    }
}
