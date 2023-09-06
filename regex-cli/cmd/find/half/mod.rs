use std::io::{stdout, Write};

use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::{HalfMatch, Input, MatchError, PatternID},
};

use crate::{
    args::{self, Configurable, Usage},
    util::{self, Table},
};

mod dfa;

pub fn run(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = r#"\
Executes a search for "half" matches. That is, matches that only have the end
offset of a match (as well as the pattern that matched). This type of search
can be done by any regex engine, but for this command, we only expose regex
engines that can specifically do less work to report a half match.

For example, a DFA regex can report a half match by only searching with its
forward DFA. Indeed, if all we care about are half matches, we can avoid
compiling a reverse DFA entirely.

Also, since half matches cannot know the full bounds of a match, this only
prints the end offset of each match and not the matched contents. (Since the
start offset of each match is not known.)

USAGE:
    regex-cli find half <engine>

ENGINES:
    dense      Search with the dense DFA regex engine.
    hybrid     Search with the lazy DFA regex engine.
    meta       Search with the meta regex engine.
    regex      Search with the top-level API regex engine.
    sparse     Search with the sparse DFA regex engine.
"#;
    let cmd = args::next_as_command(USAGE, p)?;
    match &*cmd {
        "dense" => dfa::run_dense(p),
        "hybrid" => dfa::run_hybrid(p),
        "meta" => run_meta(p),
        "regex" => run_regex(p),
        "sparse" => dfa::run_sparse(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}

#[derive(Debug, Default)]
struct Args {
    overlapping: bool,
}

impl Configurable for Args {
    fn configure(
        &mut self,
        _: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("overlapping") => {
                self.overlapping = true;
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &[Usage] = &[Usage::new(
            "--overlapping",
            "Search for overlapping matches.",
            r#"
This flag enables overlapping mode, where the regex engine will attempt to find
all possible matches reported by the underlying matcher.

Generally this flag is used in conjunction with '--match-kind all'. If the
match semantics are not set to compile all possible matches in the underlying
automaton, then the results will likely be counter-intuitive.
"#,
        )];
        USAGES
    }
}

fn run_regex(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for half matches using the top-level API regex engine.

USAGE:
    regex-cli find half regex [-p <pattern> ...] <haystack-path>
    regex-cli find half regex [-p <pattern> ...] -y <haystack>

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
            .shortest_match_at(input.haystack(), input.start())
            .map(|offset| HalfMatch::new(PatternID::ZERO, offset)))
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
Executes a search for half matches using the meta regex engine.

USAGE:
    regex-cli find half meta [-p <pattern> ...] <haystack-path>
    regex-cli find half meta [-p <pattern> ...] -y <haystack>

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

    let search = |input: &Input<'_>| Ok(re.search_half(input));
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

/// A function that takes in a bunch of configuration, runs the given search
/// routine, and prints out a table of counts.
fn run_counts(
    table: &mut Table,
    common: &args::common::Config,
    find: &super::Config,
    input: &args::input::Config,
    haystack: &args::haystack::Config,
    pattern_len: usize,
    mut search: impl FnMut(&Input<'_>) -> Result<Option<HalfMatch>, MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (counts, time) = util::timeitr(|| {
            let mut counts = vec![0; pattern_len];
            for _ in 0..find.repeat() {
                let mut it =
                    regex_automata::util::iter::Searcher::new(input.clone());
                while let Some(m) = it.try_advance_half(&mut search)? {
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
    mut search: impl FnMut(&Input<'_>) -> Result<Option<HalfMatch>, MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (matches, time) = util::timeitr(|| {
            let mut matches = vec![];
            for _ in 0..find.repeat() {
                let mut it =
                    regex_automata::util::iter::Searcher::new(input.clone());
                while let Some(m) = it.try_advance_half(&mut search)? {
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
                writeln!(out, "{}:{}", m.pattern().as_usize(), m.offset())?;
            }
        }
        Ok(())
    })
}

/// Like `run_counts`, but does an overlapping search.
fn run_counts_overlapping<S>(
    table: &mut Table,
    common: &args::common::Config,
    find: &super::Config,
    input: &args::input::Config,
    haystack: &args::haystack::Config,
    pattern_len: usize,
    mut start_state: impl FnMut() -> S,
    mut get_match: impl FnMut(&S) -> Option<HalfMatch>,
    mut search: impl FnMut(&Input<'_>, &mut S) -> Result<(), MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (counts, time) = util::timeitr(|| {
            let mut counts = vec![0; pattern_len];
            for _ in 0..find.repeat() {
                let mut state = start_state();
                loop {
                    search(&input, &mut state)?;
                    match get_match(&state) {
                        None => break,
                        Some(hm) => counts[hm.pattern().as_usize()] += 1,
                    }
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

/// Like `run_search`, but does an overlapping search.
fn run_search_overlapping<S>(
    table: &mut Table,
    common: &args::common::Config,
    find: &super::Config,
    input: &args::input::Config,
    haystack: &args::haystack::Config,
    mut start_state: impl FnMut() -> S,
    mut get_match: impl FnMut(&S) -> Option<HalfMatch>,
    mut search: impl FnMut(&Input<'_>, &mut S) -> Result<(), MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (matches, time) = util::timeitr(|| {
            let mut matches = vec![];
            for _ in 0..find.repeat() {
                let mut state = start_state();
                loop {
                    search(&input, &mut state)?;
                    match get_match(&state) {
                        None => break,
                        Some(hm) => matches.push(hm),
                    }
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
                writeln!(out, "{}:{}", m.pattern().as_usize(), m.offset())?;
            }
        }
        Ok(())
    })
}
