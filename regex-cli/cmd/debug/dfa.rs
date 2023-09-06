use std::io::{stdout, Write};

use crate::{
    args,
    util::{self, Table},
};

use {lexopt, regex_automata::dfa::Automaton};

pub fn run_dense(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a dense DFA or a dense DFA regex.

A DFA regex contains two DFAs: a forward DFA for finding the end of a match,
and a reverse DFA for finding the start of a match. These can be compiled
independently using just 'regex-cli debug dense dfa', but using the 'regex'
sub-command will handle it for you and print the debug representation of both
the forward and reverse DFAs.

USAGE:
    regex-cli debug dense <command> ...

COMMANDS:
    dfa    Print the debug representation of a single dense DFA.
    regex  Print the debug representation of a forward and reverse DFA regex.
";

    let cmd = args::next_as_command(USAGE, p)?;
    match &*cmd {
        "dfa" => run_dense_dfa(p),
        "regex" => run_dense_regex(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}

fn run_dense_dfa(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a fully compiled dense DFA.

USAGE:
    regex-cli debug dense dfa [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dfa = args::dfa::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut dfa,
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
    let (dfa, time) = util::timeitr(|| dfa.from_nfa(&nfa))?;
    table.add("compile dfa time", time);
    table.add("memory", dfa.memory_usage());
    table.add("pattern len", dfa.pattern_len());
    table.add("start kind", dfa.start_kind());
    table.add("alphabet len", dfa.alphabet_len());
    table.add("stride", dfa.stride());
    table.add("has empty?", dfa.has_empty());
    table.add("is utf8?", dfa.is_utf8());
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        if common.table() {
            writeln!(stdout(), "")?;
        }
        writeln!(stdout(), "{:?}", dfa)?;
    }
    Ok(())
}

fn run_dense_regex(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a fully compiled dense DFA regex.

This includes both the forward and reverse DFAs that make up a dense DFA regex.

USAGE:
    regex-cli debug dense regex [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dfa = args::dfa::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut dfa,
        ],
    )?;

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);

    let (nfafwd, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile forward nfa time", time);
    let (dfafwd, time) = util::timeitr(|| dfa.from_nfa(&nfafwd))?;
    table.add("compile forward dfa time", time);

    let (nfarev, time) =
        util::timeitr(|| thompson.reversed().from_hirs(&hirs))?;
    table.add("compile reverse nfa time", time);
    let (dfarev, time) = util::timeitr(|| dfa.reversed().from_nfa(&nfarev))?;
    table.add("compile reverse dfa time", time);

    let (re, time) = util::timeit(|| {
        regex_automata::dfa::regex::Builder::new()
            .build_from_dfas(dfafwd, dfarev)
    });
    table.add("build regex time", time);
    table.add(
        "memory",
        re.forward().memory_usage() + re.reverse().memory_usage(),
    );
    table.add("pattern len", re.pattern_len());
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        if common.table() {
            writeln!(stdout(), "")?;
        }
        writeln!(stdout(), "{:?}", re)?;
    }
    Ok(())
}

pub fn run_sparse(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a sparse DFA or a sparse DFA regex.

A DFA regex contains two DFAs: a forward DFA for finding the end of a match,
and a reverse DFA for finding the start of a match. These can be compiled
independently using just 'regex-cli debug dense dfa', but using the 'regex'
sub-command will handle it for you and print the debug representation of both
the forward and reverse DFAs.

USAGE:
    regex-cli debug sparse <command> ...

COMMANDS:
    dfa    Print the debug representation of a single sparse DFA.
    regex  Print the debug representation of a forward and reverse DFA regex.
";

    let cmd = args::next_as_command(USAGE, p)?;
    match &*cmd {
        "dfa" => run_sparse_dfa(p),
        "regex" => run_sparse_regex(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}

fn run_sparse_dfa(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a fully compiled sparse DFA.

USAGE:
    regex-cli debug sparse dfa [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dfa = args::dfa::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut dfa,
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
    let (dfa, time) = util::timeitr(|| dfa.from_nfa_sparse(&nfa))?;
    table.add("compile dfa time", time);
    table.add("memory", dfa.memory_usage());
    table.add("pattern len", dfa.pattern_len());
    table.add("start kind", dfa.start_kind());
    table.add("has empty?", dfa.has_empty());
    table.add("is utf8?", dfa.is_utf8());
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        if common.table() {
            writeln!(stdout(), "")?;
        }
        writeln!(stdout(), "{:?}", dfa)?;
    }
    Ok(())
}

fn run_sparse_regex(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Prints the debug representation of a fully compiled sparse DFA regex.

This includes both the forward and reverse DFAs that make up a sparse DFA
regex.

USAGE:
    regex-cli debug sparse regex [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dfa = args::dfa::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut dfa,
        ],
    )?;

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);

    let (nfafwd, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile forward nfa time", time);
    let (dfafwd, time) = util::timeitr(|| dfa.from_nfa_sparse(&nfafwd))?;
    table.add("compile forward dfa time", time);

    let (nfarev, time) =
        util::timeitr(|| thompson.reversed().from_hirs(&hirs))?;
    table.add("compile reverse nfa time", time);
    let (dfarev, time) =
        util::timeitr(|| dfa.reversed().from_nfa_sparse(&nfarev))?;
    table.add("compile reverse dfa time", time);

    let (re, time) = util::timeit(|| {
        regex_automata::dfa::regex::Builder::new()
            .build_from_dfas(dfafwd, dfarev)
    });
    table.add("build regex time", time);
    table.add(
        "memory",
        re.forward().memory_usage() + re.reverse().memory_usage(),
    );
    table.add("pattern len", re.pattern_len());
    if common.table() {
        table.print(stdout())?;
    }
    if !common.quiet {
        if common.table() {
            writeln!(stdout(), "")?;
        }
        writeln!(stdout(), "{:?}", re)?;
    }
    Ok(())
}
