use std::io::{stdout, Write};

use {
    anyhow::Context,
    bstr::BString,
    lexopt::{Arg, Parser, ValueExt},
    regex_syntax::hir::literal::{ExtractKind, Extractor},
};

use crate::{
    args::{self, Configurable, Usage},
    util::{self, Table},
};

pub fn run(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of extract literals from a regex pattern.

Note that the literals this command prints by default should roughly reflect
what regex-automata's meta regex engine does by default. In particular, this
will optimize the extracted literals and will do so under the presumption of
leftmost-first match semantics. The --no-optimize flag can be given to skip
this optimization step and instead get the literals precisely as they were
extracted.

USAGE:
    regex-cli debug literal <pattern>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut literal = Literal::default();
    args::configure(
        p,
        USAGE,
        &mut [&mut common, &mut patterns, &mut syntax, &mut literal],
    )?;

    let pats = patterns.get()?;
    anyhow::ensure!(
        pats.len() == 1,
        "only one pattern is allowed, but {} were given",
        pats.len(),
    );

    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (mut seq, time) = util::timeit(|| literal.extractor.extract(&hirs[0]));
    table.add("extraction time", time);
    if literal.optimize.unwrap_or(true) {
        let ((), time) = util::timeitr(|| {
            match literal.kind {
                ExtractKind::Prefix => seq.optimize_for_prefix_by_preference(),
                ExtractKind::Suffix => seq.optimize_for_suffix_by_preference(),
                unk => {
                    anyhow::bail!(
                        "unsupported literal extraction kind: {:?}",
                        unk
                    )
                }
            }
            Ok(())
        })?;
        table.add("optimization time", time);
    }
    table.add("len", seq.len());
    table.add("is finite?", seq.is_finite());
    table.add("is exact?", seq.is_exact());
    table.add("min literal len", seq.min_literal_len());
    table.add("max literal len", seq.max_literal_len());
    table.add(
        "longest common prefix",
        seq.longest_common_prefix().map(BString::from),
    );
    table.add(
        "longest common suffix",
        seq.longest_common_suffix().map(BString::from),
    );
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        let mut out = stdout();
        if common.table() {
            writeln!(out, "")?;
        }
        match seq.literals() {
            None => writeln!(out, "{:?}", seq)?,
            Some(literals) => {
                for lit in literals.iter() {
                    writeln!(stdout(), "{:?}", lit)?;
                }
            }
        }
    }
    Ok(())
}

#[derive(Debug, Default)]
struct Literal {
    extractor: Extractor,
    kind: ExtractKind,
    optimize: Option<bool>,
}

impl Configurable for Literal {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("extract-kind") => {
                let value = p.value().context("--extract-kind")?;
                let value = value.string().context("--extract-kind")?;
                let kind = match &*value {
                    "prefix" => ExtractKind::Prefix,
                    "suffix" => ExtractKind::Suffix,
                    unk => anyhow::bail!(
                        "unknown value for --extract-kind: {}",
                        unk
                    ),
                };
                self.kind = kind.clone();
                self.extractor.kind(kind);
            }
            Arg::Long("limit-class") => {
                let limit = args::parse(p, "--limit-class")?;
                self.extractor.limit_class(limit);
            }
            Arg::Long("limit-repeat") => {
                let limit = args::parse(p, "--limit-repeat")?;
                self.extractor.limit_repeat(limit);
            }
            Arg::Long("limit-literal-len") => {
                let limit = args::parse(p, "--limit-literal-len")?;
                self.extractor.limit_literal_len(limit);
            }
            Arg::Long("limit-total") => {
                let limit = args::parse(p, "--limit-total")?;
                self.extractor.limit_total(limit);
            }
            Arg::Long("no-optimize") => {
                self.optimize = Some(false);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "--extract-kind <kind>",
                "The literals to extract, either 'prefix' or 'suffix'.",
                r#"
This sets the kind of literals to extract. Either 'prefix' literals can be
extracted (the default) or 'suffix' literals.
"#,
            ),
            Usage::new(
                "--limit-class <limit>",
                "The maximum size of a character class to support.",
                r#"
This limit controls how big a character class needs to be for literal
extraction to ignore it. In practice, large classes aren't good for literal
extraction because it becomes easy to create very large sets of literals that
aren't practical to search quickly for.
"#,
            ),
            Usage::new(
                "--limit-repeat <limit>",
                "The maximum repeat size to expand.",
                r#"
Literal extraction will attempt to expand bounded repetitions like
'(abcd){50}'. But repetitions can become large easily, and so it makes sense to
stop at a certain point. This limit specifies that point.
"#,
            ),
            Usage::new(
                "--limit-literal-len <limit>",
                "The maximum length of a literal to extract.",
                r#"
This limit caps the maximum length of a literal that is extract. If a literal
would otherwise get longer than this limit, then it is cut and prevented from
being expanded upon.
"#,
            ),
            Usage::new(
                "--limit-total <limit>",
                "Limits the total number of literals to extract.",
                r#"
This limit applies to the total number of literals to extract. If the number of
literals would exceed this number, then literal extraction may use heuristics
to cut the set before proceeding. In some cases though, this may cause
extraction to give up entirely and return no literals.

This limit tends to act as a backstop catch-all for when other limits fail.
For example, '[ab]{3}{3}' uses small bounded repetitions and a small character
class. The actual literals it generates are also pretty small. But the
number of total literals it creates is quite large (512) despite each of its
constituent pieces being quite small. Thus, this limit protects against cases
like these by preventing the total size of the extracted literal set from
getting too big.
"#,
            ),
            Usage::new(
                "--no-optimize",
                "Don't attempt to optimize the extracted literals.",
                r#"
This flag disables "optimization" of the extracted literals. Optimization is
performed by default as it reflects what the meta regex engine does by default.

Optimization is the "black magic" part of literal extraction that uses
heuristics to guess at what kinds of literal sets are better to search for.
Generally speaking, you want a small number of a literals to make multiple
substring vector algorithms faster, but you want your literals to be longer so
that they're more discriminatory and overall reduce their false positive rate.
"#,
            ),
        ];
        USAGES
    }
}
