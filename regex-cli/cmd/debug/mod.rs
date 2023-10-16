use std::io::{stdout, Write};

use crate::{
    args,
    util::{self, Table},
};

mod dfa;
mod literal;

pub fn run(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of various things from regex-automata and
regex-syntax.

This is useful for ad hoc interactions with objects on the command line. In
general, most objects support the full suite of configuration available in code
via the crate.

USAGE:
    regex-cli debug <command> ...

COMMANDS:
    ast        Print the debug representation of an AST.
    dense      Print the debug representation of a dense DFA.
    hir        Print the debug representation of an HIR.
    literal    Print the debug representation of extracted literals.
    onepass    Print the debug representation of a one-pass DFA.
    sparse     Print the debug representation of a sparse DFA.
    thompson   Print the debug representation of a Thompson NFA.
";

    let cmd = args::next_as_command(USAGE, p)?;
    match &*cmd {
        "ast" => run_ast(p),
        "dense" => dfa::run_dense(p),
        "hir" => run_hir(p),
        "literal" => literal::run(p),
        "onepass" => run_onepass(p),
        "sparse" => dfa::run_sparse(p),
        "thompson" => run_thompson(p),
        unk => anyhow::bail!("unrecognized command '{unk}'"),
    }
}

fn run_ast(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of an abstract syntax tree (AST).

USAGE:
    regex-cli debug ast <pattern>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    args::configure(p, USAGE, &mut [&mut common, &mut patterns, &mut syntax])?;

    let pats = patterns.get()?;
    anyhow::ensure!(
        pats.len() == 1,
        "only one pattern is allowed, but {} were given",
        pats.len(),
    );

    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        if common.table() {
            writeln!(stdout(), "")?;
        }
        writeln!(stdout(), "{:#?}", &asts[0])?;
    }
    Ok(())
}

fn run_hir(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a high-level intermediate representation
(HIR).

USAGE:
    regex-cli debug hir <pattern>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    args::configure(p, USAGE, &mut [&mut common, &mut patterns, &mut syntax])?;

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
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        if common.table() {
            writeln!(stdout(), "")?;
        }
        writeln!(stdout(), "{:#?}", &hirs[0])?;
    }
    Ok(())
}

fn run_onepass(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a one-pass DFA.

USAGE:
    regex-cli debug onepass [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut onepass = args::onepass::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut onepass,
        ],
    )?;

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfa, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile nfa time", time);
    let (dfa, time) = util::timeitr(|| onepass.from_nfa(&nfa))?;
    table.add("compile one-pass DFA time", time);
    table.add("memory", dfa.memory_usage());
    table.add("states", dfa.state_len());
    table.add("pattern len", dfa.pattern_len());
    table.add("alphabet len", dfa.alphabet_len());
    table.add("stride", dfa.stride());
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        if common.table() {
            writeln!(stdout(), "")?;
        }
        writeln!(stdout(), "{dfa:?}")?;
    }
    Ok(())
}

fn run_thompson(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a Thompson NFA.

USAGE:
    regex-cli debug thompson [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [&mut common, &mut patterns, &mut syntax, &mut thompson],
    )?;

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfa, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile nfa time", time);
    table.add("memory", nfa.memory_usage());
    table.add("states", nfa.states().len());
    table.add("pattern len", nfa.pattern_len());
    table.add("capture len", nfa.group_info().all_group_len());
    table.add("has empty?", nfa.has_empty());
    table.add("is utf8?", nfa.is_utf8());
    table.add("is reverse?", nfa.is_reverse());
    table.add(
        "line terminator",
        bstr::BString::from(&[nfa.look_matcher().get_line_terminator()][..]),
    );
    table.add("lookset any", nfa.look_set_any());
    table.add("lookset prefix any", nfa.look_set_prefix_any());
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        if common.table() {
            writeln!(stdout(), "")?;
        }
        writeln!(stdout(), "{nfa:?}")?;
    }
    Ok(())
}
