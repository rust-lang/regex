use std::io::{stdout, Write};

use {
    anyhow::Context,
    bstr::ByteSlice,
    lexopt::Parser,
    regex_automata::{Input, Match, MatchError, PatternID},
};

use crate::{
    args,
    util::{self, Table},
};

mod dfa;
mod nfa;

pub fn run(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search.

USAGE:
    regex-cli find match <engine>

ENGINES:
    backtrack  Search with the bounded backtracker regex engine.
    dense      Search with the dense DFA regex engine.
    hybrid     Search with the lazy DFA regex engine.
    lite       Search with the regex-lite engine.
    meta       Search with the meta regex engine.
    onepass    Search with the one-pass DFA regex engine.
    pikevm     Search with the PikeVM regex engine.
    regex      Search with the top-level API regex engine.
    sparse     Search with the sparse DFA regex engine.
";
    let cmd = args::next_as_command(USAGE, p)?;
    match &*cmd {
        "backtrack" => nfa::run_backtrack(p),
        "dense" => dfa::run_dense(p),
        "hybrid" => dfa::run_hybrid(p),
        "lite" => run_lite(p),
        "meta" => run_meta(p),
        "onepass" => dfa::run_onepass(p),
        "pikevm" => nfa::run_pikevm(p),
        "regex" => run_regex(p),
        "sparse" => dfa::run_sparse(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}

fn run_regex(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the top-level API regex engine.

USAGE:
    regex-cli find match regex [-p <pattern> ...] <haystack-path>
    regex-cli find match regex [-p <pattern> ...] -y <haystack>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::only_flags();
    let mut haystack = args::haystack::Config::default();
    let mut syntax = args::syntax::Config::default();
    let mut api = args::api::Config::default();
    let mut find = super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut api,
            &mut find,
        ],
    )?;

    let pats = patterns.get()?;
    let syn = syntax.syntax()?;
    let mut table = Table::empty();
    let (re, time) = util::timeitr(|| api.from_patterns(&syn, &pats))?;
    table.add("build regex time", time);

    // The top-level API doesn't support regex-automata's more granular Input
    // abstraction.
    let input = args::input::Config::default();
    let search = |input: &Input<'_>| {
        Ok(re
            .find_at(input.haystack(), input.start())
            .map(|m| Match::new(PatternID::ZERO, m.start()..m.end())))
    };
    if find.count {
        run_counts(&mut table, &common, &find, &input, &haystack, 1, search)?;
    } else {
        run_search(&mut table, &common, &find, &input, &haystack, search)?;
    }
    Ok(())
}

fn run_meta(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the meta regex engine.

USAGE:
    regex-cli find match meta [-p <pattern> ...] <haystack-path>
    regex-cli find match meta [-p <pattern> ...] -y <haystack>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut input = args::input::Config::default();
    let mut patterns = args::patterns::Config::only_flags();
    let mut haystack = args::haystack::Config::default();
    let mut syntax = args::syntax::Config::default();
    let mut meta = args::meta::Config::default();
    let mut find = super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut input,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut meta,
            &mut find,
        ],
    )?;

    let pats = patterns.get()?;
    let mut table = Table::empty();

    let re = if meta.build_from_patterns() {
        let (re, time) = util::timeitr(|| meta.from_patterns(&syntax, &pats))?;
        table.add("build meta time", time);
        re
    } else {
        let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
        table.add("parse time", time);
        let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
        table.add("translate time", time);
        let (re, time) = util::timeitr(|| meta.from_hirs(&hirs))?;
        table.add("build meta time", time);
        re
    };

    let search = |input: &Input<'_>| Ok(re.search(input));
    if find.count {
        run_counts(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            re.pattern_len(),
            search,
        )?;
    } else {
        run_search(&mut table, &common, &find, &input, &haystack, search)?;
    }
    Ok(())
}

fn run_lite(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the top-level regex-lite engine.

Note that since the regex-lite crate doesn't have an API for search arbitrary
byte slices, the haystack must be valid UTF-8. If it isn't, this command will
report an error.

USAGE:
    regex-cli find match lite [-p <pattern> ...] <haystack-path>
    regex-cli find match lite [-p <pattern> ...] -y <haystack>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::only_flags();
    let mut haystack = args::haystack::Config::default();
    let mut syntax = args::syntax::Config::default();
    let mut lite = args::lite::Config::default();
    let mut find = super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut lite,
            &mut find,
        ],
    )?;

    let pats = patterns.get()?;
    let syn = syntax.syntax()?;
    let mut table = Table::empty();
    let (re, time) = util::timeitr(|| lite.from_patterns(&syn, &pats))?;
    table.add("build regex time", time);

    // Check that the haystack is valid UTF-8 since regex-lite doesn't support
    // searching arbitrary byte sequences. (At time of writing.)
    haystack.get()?.to_str()?;

    // The top-level regex-lite API doesn't support regex-automata's more
    // granular Input abstraction.
    let input = args::input::Config::default();
    let search = |input: &Input<'_>| {
        let haystack = input.haystack().to_str().unwrap();
        Ok(re
            .find_at(haystack, input.start())
            .map(|m| Match::new(PatternID::ZERO, m.start()..m.end())))
    };
    if find.count {
        run_counts(&mut table, &common, &find, &input, &haystack, 1, search)?;
    } else {
        run_search(&mut table, &common, &find, &input, &haystack, search)?;
    }
    Ok(())
}

/// A function that takes in a bunch of configuration, runs the given search
/// routine, and prints out a table of counts.
fn run_counts(
    table: &mut Table,
    common: &args::common::Config,
    find: &super::Config,
    input: &args::input::Config,
    haystack: &args::haystack::Config,
    pattern_len: usize,
    mut search: impl FnMut(&Input<'_>) -> Result<Option<Match>, MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (counts, time) = util::timeitr(|| {
            let mut counts = vec![0; pattern_len];
            for _ in 0..find.repeat() {
                let mut it =
                    regex_automata::util::iter::Searcher::new(input.clone());
                while let Some(m) = it.try_advance(&mut search)? {
                    counts[m.pattern().as_usize()] += 1;
                }
            }
            Ok::<_, anyhow::Error>(counts)
        })?;
        table.add("search time", time);
        table.add("total matches", counts.iter().copied().sum::<u64>());
        if common.table() {
            table.print(&mut out)?;
        }
        if !common.quiet {
            for (i, &count) in counts.iter().enumerate() {
                let pid = PatternID::new(i).context("invalid pattern ID")?;
                writeln!(out, "{}:{}", pid.as_usize(), count)?;
            }
        }
        Ok(())
    })
}

/// Like `run_counts`, but prints the actual matches instead.
fn run_search(
    table: &mut Table,
    common: &args::common::Config,
    find: &super::Config,
    input: &args::input::Config,
    haystack: &args::haystack::Config,
    mut search: impl FnMut(&Input<'_>) -> Result<Option<Match>, MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (matches, time) = util::timeitr(|| {
            let mut matches = vec![];
            for _ in 0..find.repeat() {
                let mut it =
                    regex_automata::util::iter::Searcher::new(input.clone());
                while let Some(m) = it.try_advance(&mut search)? {
                    matches.push(m);
                }
            }
            Ok::<_, anyhow::Error>(matches)
        })?;
        table.add("search time", time);
        table.add("total matches", matches.len());
        if common.table() {
            table.print(&mut out)?;
        }
        if !common.quiet {
            for m in matches.iter() {
                writeln!(
                    out,
                    "{}:{}:{}:{}",
                    m.pattern().as_usize(),
                    m.start(),
                    m.end(),
                    input.haystack()[m.range()].escape_bytes()
                )?;
            }
        }
        Ok(())
    })
}
