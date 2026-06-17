use std::path::PathBuf;

use {
    anyhow::Context,
    lexopt::{Arg, Parser, ValueExt},
};

use crate::args::{Configurable, Usage};

/// A configuration object for reading patterns from the command line.
///
/// It supports two different modes. One mode limits it to reading patterns
/// from optional flags, i.e., `-p/--pattern` and `-f/--pattern-file`. The
/// other mode permits reading from optional flags, but also reads all
/// positional arguments as patterns too. The latter is convenient in cases
/// where patterns are the only input to a command (like `regex-cli debug`).
#[derive(Debug, Default)]
pub struct Config {
    patterns: Vec<String>,
    fixed_strings: bool,
    combine: bool,
    mode: Mode,
}

impl Config {
    /// Creates a new configuration that will greedily treat every positional
    /// argument as a pattern. This also supports all other ways of providing
    /// patterns, i.e., the `-p/--pattern` and `-f/--file` flags.
    ///
    /// This is useful for commands that don't accept any other kind of
    /// positional arguments.
    pub fn positional() -> Config {
        Config { mode: Mode::Positional, ..Config::default() }
    }

    /// Creates a new configuration that will never treat a positional argument
    /// as a pattern. Instead, it only reads patterns from the `-p/--pattern`
    /// and `-f/--file` flags.
    ///
    /// This is useful for commands that accept other kinds of positional
    /// arguments. Forcing the use of a flag helps avoid resolving more
    /// complicated ambiguities regarding how to treat each positional
    /// argument.
    ///
    /// This is equivalent to `Config::default()`.
    pub fn only_flags() -> Config {
        Config::default()
    }

    /// Returns all of the pattern strings from this configuration, escaping
    /// and joining them if requested. When joining is requested, then at most
    /// one pattern is returned.
    ///
    /// Note that it is legal for this to return zero patterns!
    pub fn get(
        &self,
        syntax: &crate::args::syntax::Config,
    ) -> anyhow::Result<Vec<String>> {
        let mut pats = self.patterns.clone();
        if self.fixed_strings {
            pats = pats.iter().map(|p| regex_syntax::escape(p)).collect();
        }
        if self.combine {
            syntax.asts(&pats)?;
            pats = vec![pats.join("|")];
        }
        Ok(pats)
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Short('p') | Arg::Long("pattern") => {
                let pat = p.value().context("-p/--pattern needs a value")?;
                let pat = pat
                    .string()
                    .context("-p/--pattern must be valid UTF-8")?;
                self.patterns.push(pat);
            }
            Arg::Short('F') | Arg::Long("fixed-strings") => {
                self.fixed_strings = true;
            }
            Arg::Short('f') | Arg::Long("pattern-file") => {
                let path =
                    PathBuf::from(p.value().context("-f/--pattern-file")?);
                let contents =
                    std::fs::read_to_string(&path).with_context(|| {
                        anyhow::anyhow!("failed to read {}", path.display())
                    })?;
                self.patterns.extend(contents.lines().map(|x| x.to_string()));
            }
            Arg::Long("combine-patterns") => {
                self.combine = true;
            }
            Arg::Value(ref mut v) => {
                if !matches!(self.mode, Mode::Positional) {
                    return Ok(false);
                }
                let v = std::mem::take(v);
                self.patterns
                    .push(v.string().context("patterns must be valid UTF-8")?);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "-p, --pattern <pattern>",
                "Add a pattern to this command.",
                r#"
This adds a new pattern to the command.

All of the patterns provided, whether by this flag, as a positional argument
(if supported) or via the -f/--pattern-file flag are combined into one regex
matcher.

All patterns given must be valid UTF-8.
"#,
            ),
            Usage::new(
                "-f, --pattern-file",
                "Read patterns from the file given.",
                r#"
Reads patterns, one per line, from the file given.

All of the patterns provided, whether by this flag, as a positional argument
(if supported) or via the -p/--pattern flag are combined into one regex
matcher.

All patterns given must be valid UTF-8.
"#,
            ),
            Usage::new(
                "-F, --fixed-strings",
                "Interpret all patterns literally.",
                r#"
When set, all patterns are interpreted as literal strings. So for example,
special regex meta characters like '+' are matched literally instead of being
given special significance.
"#,
            ),
            Usage::new(
                "--combine-patterns",
                "Combine all patterns into one via an alternation.",
                r#"
This flag combines all patterns given in this command into one by joining
them together via a '|'. This is useful in cases where you want to provide
many different things to search for, but explicitly only want one pattern to
be constructed. In terms of debugging the regex engine, this can be used to
inspect the practical differences between multi-pattern regexes and single
pattern regexes, even when they generally match the same thing.
"#,
            ),
        ];
        USAGES
    }
}

/// The parsing behavior of a pattern configuration. That is, either treat
/// positional arguments as patterns or not.
///
/// The default is to only parse patterns from flags.
#[derive(Debug)]
enum Mode {
    Positional,
    OnlyFlags,
}

impl Default for Mode {
    fn default() -> Mode {
        Mode::OnlyFlags
    }
}

#[cfg(test)]
mod tests {
    use super::Config;

    #[test]
    fn combine_patterns_rejects_individually_invalid_patterns() {
        let config = Config {
            patterns: vec!["ab(cd".to_string(), "ef)gh".to_string()],
            combine: true,
            ..Config::default()
        };

        assert!(config.get(&crate::args::syntax::Config::default()).is_err());
    }

    #[test]
    fn combine_patterns_accepts_individually_valid_patterns() {
        let config = Config {
            patterns: vec!["ab(cd)".to_string(), "ef(gh)".to_string()],
            combine: true,
            ..Config::default()
        };

        let got = config.get(&crate::args::syntax::Config::default()).unwrap();
        assert_eq!(got, vec!["ab(cd)|ef(gh)".to_string()]);
    }

    #[test]
    fn combine_patterns_respects_syntax_configuration() {
        let mut syntax = crate::args::syntax::Config::default();
        let config = Config {
            patterns: vec![r"\141".to_string(), r"\142".to_string()],
            combine: true,
            ..Config::default()
        };

        assert!(config.get(&syntax).is_err());
        let mut parser = lexopt::Parser::from_args(["--octal"]);
        crate::args::configure(
            &mut parser,
            "",
            &mut [&mut syntax as &mut dyn crate::args::Configurable],
        )
        .unwrap();
        let got = config.get(&syntax).unwrap();
        assert_eq!(got, vec![r"\141|\142".to_string()]);
    }
}
