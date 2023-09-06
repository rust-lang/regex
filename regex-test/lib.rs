/*!
A crate for defining tests in a TOML format and applying them to regex engine
implementations.

Generally speaking, if you aren't writing your own regex engine and looking to
test it, then this crate is probably not for you. Moreover, this crate does not
come with any actual tests. It merely defines the test format and provides some
convenient routines for executing tests within the context of Rust unit tests.

# Format

The entire test corpus is derived from zero or more TOML files. Each TOML
file contains zero or more tests, where each test is defined as a table via
`[[test]]`.

Each test has the following fields:

* `name` - A name for the test. It must be unique within its file. A test's
[`RegexTest::full_name`] is derived either via `{group_name}/{name}` or
`{group_name}/{name}/{additional_name}`, with the latter being used only when
[`TestRunner::expand`] is used. The `group_name` is derived from the file stem
(the file name without the `.toml suffix).
* `regex` - The regex to test. This is either a string or a (possibly empty)
list of regex patterns. When using a list, the underlying regex engine is
expected to support multiple patterns where each are identified starting from
`0` and incrementing by 1 for each subsequent pattern.
* `haystack` - The text to search.
* `bounds` - An optional field whose value is a table with `start` and `end`
fields, whose values must be valid for the given `haystack`. When set,
the search will only execute within these bounds. When absent, the bounds
correspond to `start = 0` and `end = haystack.len()`.
* `matches` - Zero or more match values. Each match value can be in one of four
formats:
    * A simple span, i.e., `[5, 12]`, corresponding to the start and end of the
    match, in byte offsets. The start is inclusive and the end is exclusive.
    The pattern ID for the match is assumed to be `0`.
    * A table corresponding to the matching pattern ID and the span of the
    match. For example, `{ id = 5, span = [20, 21] }`.
    * A list of capture group spans, with the first corresponding to the
    overall match and the pattern ID assumed to be `0`. For example,
    `[[5, 10], [6, 8], [], [9, 10]]`, where `[]` corresponds to a group
    present in the regex but one that did not participate in a match.
    * A table corresponding to the matching pattern ID and a list of spans
    corresponding to the capture groups. For example,
    `{ id = 5, spans = [[5, 10], [6, 8], [], [9, 10]] }`. This is the most
    general, but also most verbose, syntax.
* `match-limit` - An optional field that specifies a limit on the number of
matches. When absent, no limit is enforced and all matches should be reported
by the regex engine. This can be useful, for example, when one only cares about
the first match.
* `compiles` - An optional field indicating whether the regex is expected to
compile. It defaults to `true` when absent. When `true`, if the regex does not
compile, then the test fails. Conversely, when `false`, if the regex _does_
compile, then the test fails.
* `anchored` - Whether to execute an anchored search or not. Note that this is
not the same as adding a `^` to the beginning of your regex pattern. `^` always
requires the regex to match at position `0`, but an anchored search simply
requires that the regex match at the starting position of the search. (The
starting position of the search can be configured via the optional `bounds`
field.)
* `case-insensitive` - Whether to match the regex case insensitively. This is
disabled by default. There is no real difference between using this field and
adding a `(?i)` to the beginning of your regex. (Some regex engines may not
support `(?i)`.)
* `unescape` - When enabled, the haystack is unescaped. Sequences like `\x00`
are turned into their corresponding byte values. This permits one to write
haystacks that contain invalid UTF-8 without embedding actual invalid UTF-8
into a TOML file (which is not allowed). There is generally no other reason to
enable `unescape`.
* `unicode` - When enabled, the regex pattern should be compiled with its
corresponding Unicode mode enabled. For example, `[^a]` matches any UTF-8
encoding of any codepoint other than `a`. Case insensitivty should be Unicode
aware. Unicode classes like `\pL` are available. The Perl classes `\w`, `\s`
and `\d` should be Unicode aware. And so on. This is an optional field and is
enabled by default.
* `utf8` - When this is enabled, all regex match substrings should be entirely
valid UTF-8. While parts of the haystack the regex searches through may not be
valid UTF-8, only the portions that are valid UTF-8 may be reported in match
spans. Importantly, this includes zero-width matches. Zero-width matches must
never split the UTF-8 encoding of a single codepoint when this is enabled. This
is an optional field and is enabled by default.
* `line-terminator` - This sets the line terminator used by the multi-line
assertions `(?m:^)` and `(?m:$)`. It defaults to `\n`. It must be exactly one
byte. This field is automatically unescaped in order to permit a non-ASCII
byte.
* `match-kind` - May be one of `all`, `leftmost-first` or `leftmost-longest`.
See [`MatchKind`] for more details. This is an optional field and defaults to
`leftmost-first`.
* `search-kind` - May be one of `earliest`, `leftmost` or `overlapping`. See
[`SearchKind`] for more details. This is an optional field and defaults to
`leftmost`.
*/

#![deny(missing_docs)]

/// For convenience, `anyhow::Error` is used to represents errors in this
/// crate.
///
/// For this reason, `anyhow` is a public dependency and is re-exported here.
pub extern crate anyhow;

use std::{
    borrow::Borrow, collections::HashSet, convert::TryFrom, fs, path::Path,
};

use {
    anyhow::{bail, Context, Result},
    bstr::{BString, ByteSlice, ByteVec},
    serde::Deserialize,
};

const ENV_REGEX_TEST: &str = "REGEX_TEST";
const ENV_REGEX_TEST_VERBOSE: &str = "REGEX_TEST_VERBOSE";

/// A collection of regex tests.
#[derive(Clone, Debug, Deserialize)]
pub struct RegexTests {
    /// 'default' permits an empty TOML file.
    #[serde(default, rename = "test")]
    tests: Vec<RegexTest>,
    #[serde(skip)]
    seen: HashSet<String>,
}

impl RegexTests {
    /// Create a new empty collection of glob tests.
    pub fn new() -> RegexTests {
        RegexTests { tests: vec![], seen: HashSet::new() }
    }

    /// Loads all of the tests in the given TOML file. The group name assigned
    /// to each test is the stem of the file name. For example, if one loads
    /// `foo/bar.toml`, then the group name for each test will be `bar`.
    pub fn load<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        let path = path.as_ref();
        let data = fs::read(path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        let group_name = path
            .file_stem()
            .with_context(|| {
                format!("failed to get file name of {}", path.display())
            })?
            .to_str()
            .with_context(|| {
                format!("invalid UTF-8 found in {}", path.display())
            })?;
        self.load_slice(&group_name, &data)
            .with_context(|| format!("error loading {}", path.display()))?;
        Ok(())
    }

    /// Load all of the TOML encoded tests in `data` into this collection.
    /// The given group name is assigned to all loaded tests.
    pub fn load_slice(&mut self, group_name: &str, data: &[u8]) -> Result<()> {
        let data = std::str::from_utf8(&data).with_context(|| {
            format!("data in {} is not valid UTF-8", group_name)
        })?;
        let mut index = 1;
        let mut tests: RegexTests =
            toml::from_str(&data).with_context(|| {
                format!("error decoding TOML for '{}'", group_name)
            })?;
        for t in &mut tests.tests {
            t.group = group_name.to_string();
            if t.name.is_empty() {
                t.name = format!("{}", index);
                index += 1;
            }
            t.full_name = format!("{}/{}", t.group, t.name);
            if t.unescape {
                t.haystack = BString::from(Vec::unescape_bytes(
                    // OK because TOML requires valid UTF-8.
                    t.haystack.to_str().unwrap(),
                ));
            }
            if t.line_terminator.is_empty() {
                t.line_terminator = BString::from("\n");
            } else {
                t.line_terminator = BString::from(Vec::unescape_bytes(
                    // OK because TOML requires valid UTF-8.
                    t.line_terminator.to_str().unwrap(),
                ));
                anyhow::ensure!(
                    t.line_terminator.len() == 1,
                    "line terminator '{:?}' has length not equal to 1",
                    t.line_terminator,
                );
            }
            if self.seen.contains(t.full_name()) {
                bail!("found duplicate tests for name '{}'", t.full_name());
            }
            self.seen.insert(t.full_name().to_string());
        }
        self.tests.extend(tests.tests);
        Ok(())
    }

    /// Return an iterator over all regex tests that have been loaded. The
    /// order of the iterator corresponds to the order in which the tests were
    /// loaded.
    ///
    /// This is useful to pass to [`TestRunner::test_iter`].
    pub fn iter(&self) -> RegexTestsIter {
        RegexTestsIter(self.tests.iter())
    }
}

/// A regex test describes the inputs and expected outputs of a regex match.
///
/// Each `RegexTest` represents a single `[[test]]` table in a TOML test file.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RegexTest {
    #[serde(skip)]
    group: String,
    #[serde(default)]
    name: String,
    #[serde(skip)]
    additional_name: String,
    #[serde(skip)]
    full_name: String,
    regex: RegexesFormat,
    haystack: BString,
    bounds: Option<Span>,
    matches: Vec<Captures>,
    #[serde(rename = "match-limit")]
    match_limit: Option<usize>,
    #[serde(default = "default_true")]
    compiles: bool,
    #[serde(default)]
    anchored: bool,
    #[serde(default, rename = "case-insensitive")]
    case_insensitive: bool,
    #[serde(default)]
    unescape: bool,
    #[serde(default = "default_true")]
    unicode: bool,
    #[serde(default = "default_true")]
    utf8: bool,
    #[serde(default, rename = "line-terminator")]
    line_terminator: BString,
    #[serde(default, rename = "match-kind")]
    match_kind: MatchKind,
    #[serde(default, rename = "search-kind")]
    search_kind: SearchKind,
}

impl RegexTest {
    /// Return the group name of this test.
    ///
    /// Usually the group name corresponds to a collection of related
    /// tests. More specifically, when using [`RegexTests::load`], the
    /// group name corresponds to the file stem (the file name without the
    /// `.toml` suffix). Otherwise, the group name is whatever is given to
    /// [`RegexTests::load_slice`].
    pub fn group(&self) -> &str {
        &self.group
    }

    /// The name of this test.
    ///
    /// Note that this is only the name as given in the `[[test]]` block. The
    /// actual full name used for filtering and reporting can be retrieved with
    /// [`RegexTest::full_name`].
    pub fn name(&self) -> &str {
        &self.name
    }

    /// The additional name for this test.
    ///
    /// This is only non-empty when the test runner was expanded with
    /// [`TestRunner::expand`].
    pub fn additional_name(&self) -> &str {
        &self.additional_name
    }

    /// The full name of this test, which is formed by joining the group
    /// name, the test name and the additional name with a `/`.
    pub fn full_name(&self) -> &str {
        &self.full_name
    }

    /// Return all of the regexes that should be matched for this test. This
    /// slice may be empty!
    pub fn regexes(&self) -> &[String] {
        self.regex.patterns()
    }

    /// Return the bytes on which the regex should be matched.
    pub fn haystack(&self) -> &[u8] {
        &self.haystack
    }

    /// Returns the bounds of a search.
    ///
    /// If the test didn't specify any bounds, then the bounds returned are
    /// equivalent to the entire haystack.
    pub fn bounds(&self) -> Span {
        self.bounds.unwrap_or(Span { start: 0, end: self.haystack().len() })
    }

    /// Returns the limit on the number of matches that should be reported,
    /// if specified in the test.
    ///
    /// This is useful for tests that only want to check for the first
    /// match. In which case, the match limit is set to 1.
    ///
    /// If there is no match limit, then regex engines are expected to report
    /// all matches.
    pub fn match_limit(&self) -> Option<usize> {
        self.match_limit
    }

    /// Returns true if the regex(es) in this test are expected to compile.
    pub fn compiles(&self) -> bool {
        self.compiles
    }

    /// Whether the regex should perform an anchored search.
    ///
    /// This is distinct from putting a `^` in the regex in that `bounds` may
    /// be specified that permit the regex search to start at a position
    /// `i > 0`. In which case, enabling anchored mode here requires that any
    /// matches produced must have a start offset at `i`.
    pub fn anchored(&self) -> bool {
        self.anchored
    }

    /// Returns true if regex matching should be performed without regard to
    /// case.
    pub fn case_insensitive(&self) -> bool {
        self.case_insensitive
    }

    /// Returns true if regex matching should have Unicode mode enabled.
    ///
    /// For example, `[^a]` matches any UTF-8 encoding of any codepoint other
    /// than `a`. Case insensitivty should be Unicode aware. Unicode classes
    /// like `\pL` are available. The Perl classes `\w`, `\s` and `\d` should
    /// be Unicode aware. And so on.
    ///
    /// This is enabled by default.
    pub fn unicode(&self) -> bool {
        self.unicode
    }

    /// Returns true if regex matching should exclusively match valid UTF-8.
    /// When this is disabled, matching on arbitrary bytes is permitted.
    ///
    /// When this is enabled, all regex match substrings should be entirely
    /// valid UTF-8. While parts of the haystack the regex searches through
    /// may not be valid UTF-8, only the portions that are valid UTF-8 may be
    /// reported in match spans.
    ///
    /// Importantly, this includes zero-width matches. Zero-width matches must
    /// never split the UTF-8 encoding of a single codepoint when this is
    /// enabled.
    ///
    /// This is enabled by default.
    pub fn utf8(&self) -> bool {
        self.utf8
    }

    /// Returns the line terminator that should be used for the multi-line
    /// assertions `(?m:^)` and `(?m:$)`.
    ///
    /// If it isn't set, then this defaults to `\n`.
    pub fn line_terminator(&self) -> u8 {
        self.line_terminator[0]
    }

    /// Return the match semantics required by this test.
    pub fn match_kind(&self) -> MatchKind {
        self.match_kind
    }

    /// Return the search semantics required by this test.
    pub fn search_kind(&self) -> SearchKind {
        self.search_kind
    }

    /// Run the test and return the result produced by the given compiled
    /// regex.
    fn test(&self, regex: &mut CompiledRegex) -> TestResult {
        match regex.matcher {
            None => TestResult::skip(),
            Some(ref mut match_regex) => match_regex(self),
        }
    }

    /// Append `/name` to the `full_name` of this test.
    ///
    /// This is used to support [`TestRunner::expand`].
    fn with_additional_name(&self, name: &str) -> RegexTest {
        let additional_name = name.to_string();
        let full_name = format!("{}/{}", self.full_name, additional_name);
        RegexTest { additional_name, full_name, ..self.clone() }
    }

    /// Returns true if and only if this test expects at least one of the
    /// regexes to match the haystack.
    fn is_match(&self) -> bool {
        !self.matches.is_empty()
    }

    /// Returns a slice of pattern IDs that are expected to match the haystack.
    /// The slice is empty if no match is expected to occur. The IDs returned
    /// are deduplicated and sorted in ascending order.
    fn which_matches(&self) -> Vec<usize> {
        let mut seen = HashSet::new();
        let mut ids = vec![];
        for cap in self.matches.iter() {
            if !seen.contains(&cap.id) {
                seen.insert(cap.id);
                ids.push(cap.id);
            }
        }
        ids.sort();
        ids
    }

    /// Extracts the overall match from each `Captures` match in this test
    /// and returns it.
    fn matches(&self) -> Vec<Match> {
        let mut matches = vec![];
        for cap in self.matches.iter() {
            matches.push(cap.to_match());
        }
        matches
    }

    /// Returns the matches expected by this test, including the spans of any
    /// matching capture groups.
    fn captures(&self) -> Vec<Captures> {
        self.matches.clone()
    }
}

/// The result of compiling a regex.
///
/// In many implementations, the act of matching a regex can be separated from
/// the act of compiling a regex. A `CompiledRegex` represents a regex that has
/// been compiled and is ready to be used for matching.
///
/// The matching implementation is represented by a closure that accepts a
/// [`&RegexTest`](RegexTest) and returns a [`TestResult`].
pub struct CompiledRegex {
    matcher: Option<Box<dyn FnMut(&RegexTest) -> TestResult + 'static>>,
}

impl CompiledRegex {
    /// Provide a closure that represents the compiled regex and executes a
    /// regex match on any `RegexTest`. The `RegexTest` given to the closure
    /// provided is the exact same `RegexTest` that is used to compile this
    /// regex.
    pub fn compiled(
        matcher: impl FnMut(&RegexTest) -> TestResult + 'static,
    ) -> CompiledRegex {
        CompiledRegex { matcher: Some(Box::new(matcher)) }
    }

    /// Indicate that tests on this regex should be skipped. This typically
    /// occurs if the `RegexTest` requires something that an implementation
    /// does not support.
    pub fn skip() -> CompiledRegex {
        CompiledRegex { matcher: None }
    }

    /// Returns true if the test runner decided to skip the test when
    /// attempting to compile the regex.
    pub fn is_skip(&self) -> bool {
        self.matcher.is_none()
    }
}

impl std::fmt::Debug for CompiledRegex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let status = match self.matcher {
            None => "Skip",
            Some(_) => "Run(...)",
        };
        f.debug_struct("CompiledRegex").field("matcher", &status).finish()
    }
}

/// The result of executing a regex search.
///
/// When using the test runner, callers must provide a closure that takes
/// a `RegexTest` and returns a `TestResult`. The `TestResult` is meant to
/// capture the results of matching the haystack against the regex specified by
/// the `RegexTest`.
///
/// Usually this consists of one or more matches, which can be constructed via
/// `TestResult::matches` (for just overall matches) or `TestResult::captures`
/// (for matches with capture group spans). But the regex engine may also
/// report whether a match exists, or just whether a pattern matched or not.
/// That can be done via `TestResult::matched` and `TestResult::which`,
/// respectively.
#[derive(Debug, Clone)]
pub struct TestResult {
    kind: TestResultKind,
}

#[derive(Debug, Clone)]
enum TestResultKind {
    Match(bool),
    Which(Vec<usize>),
    StartEnd(Vec<Match>),
    Captures(Vec<Captures>),
    Skip,
    Fail { why: String },
}

impl TestResult {
    /// Create a test result that indicates just whether any match was found
    /// or not.
    pub fn matched(yes: bool) -> TestResult {
        TestResult { kind: TestResultKind::Match(yes) }
    }

    /// Create a test result that indicates which out of possibly many regexes
    /// matched the haystack. If `which` is empty, then this is equivalent to
    /// `TestResult::matched(false)`.
    ///
    /// Note that the iterator should consist of pattern IDs, where each
    /// ID corresponds to a pattern that matches anywhere in the haystack.
    /// Multiple patterns may match the same region of the haystack. That is,
    /// this supports overlapping matches.
    pub fn which<I: IntoIterator<Item = usize>>(it: I) -> TestResult {
        let mut which: Vec<usize> = it.into_iter().collect();
        which.sort();
        TestResult { kind: TestResultKind::Which(which) }
    }

    /// Create a test result containing a sequence of all matches in the test's
    /// haystack. This is useful when the regex engine only reports overall
    /// matches and not the spans of each matching capture group.
    ///
    /// If the sequence is empty, then this is equivalent to
    /// `TestResult::matched(false)`.
    pub fn matches<I: IntoIterator<Item = Match>>(it: I) -> TestResult {
        TestResult { kind: TestResultKind::StartEnd(it.into_iter().collect()) }
    }

    /// Create a test result containing a sequence of all capturing matches in
    /// the test's haystack. Each match is a `Captures`, and each `Captures`
    /// should include the spans of all matching capturing groups.
    ///
    /// If the sequence is empty, then this is equivalent to
    /// `TestResult::matched(false)`.
    pub fn captures<I: IntoIterator<Item = Captures>>(it: I) -> TestResult {
        TestResult { kind: TestResultKind::Captures(it.into_iter().collect()) }
    }

    /// Indicate that this test should be skipped. It will not be counted as
    /// a failure.
    pub fn skip() -> TestResult {
        TestResult { kind: TestResultKind::Skip }
    }

    /// Indicate that this test should be failed for the reason given.
    ///
    /// This is useful when a test needs to be failed for reasons that the
    /// test runner itself cannot check. That is, the test is failed by the
    /// implementation being tested.
    pub fn fail(why: &str) -> TestResult {
        TestResult { kind: TestResultKind::Fail { why: why.to_string() } }
    }
}

/// A runner for executing regex tests.
///
/// This runner is intended to be used within a Rust unit test, marked with the
/// `#[test]` attribute.
///
/// A test runner is responsible for running tests against a regex
/// implementation. It contains logic for skipping tests and collects test
/// results. Typical usage corresponds to calling [`TestRunner::test_iter`] on
/// an iterator of `RegexTest`s, and then calling `assert` once done. If any
/// tests failed, then `assert` will panic with an error message containing all
/// test failures. `assert` must be called before the test completes.
///
/// # Skipping tests
///
/// If the `REGEX_TEST` environment variable is set, then it may contain
/// a comma separated list of substrings. Each substring corresponds to a
/// whitelisted item, unless it starts with a `-`, in which case it corresponds
/// to a blacklisted item.
///
/// If there are any whitelist items, then a test's full name must contain at
/// least one of the whitelist substrings in order to be run, and does not
/// contain and blacklist substrings. If there are no whitelist substrings,
/// then a test is run only when it does not match any blacklist substrings.
///
/// The last substring that a test name matches takes precedent.
///
/// Callers may also specify explicit whitelist or blacklist substrings using
/// the corresponding methods on this type, which has the effect of always
/// having those rules in place for that specific test. For example, if you're
/// testing a search by building a DFA and then minimizing it, you may want to
/// skip tests with bigger regexes, since they could take quite some time to
/// run.
///
/// Whitelist and blacklist substrings are matched on the full name of each
/// test, which typically looks like `group_name/test_name`.
///
/// Currently there is no way to escape either a `-` or a `,` in `REGEX_TEST`.
/// This usually isn't required because test names usually don't include either
/// character.
#[derive(Debug)]
pub struct TestRunner {
    include: Vec<IncludePattern>,
    results: RegexTestResults,
    expanders: Vec<Expander>,
}

impl TestRunner {
    /// Create a new runner for executing tests.
    ///
    /// The test runner maintains a full list of tests that have succeeded,
    /// failed or been skipped. Moreover, the test runner may control which
    /// tests get run via its whitelist and blacklist.
    ///
    /// This returns an error if there was a problem reading the `REGEX_TEST`
    /// environment variable, which may be set to include or exclude tests.
    /// See the docs on `TestRunner` for its format.
    pub fn new() -> Result<TestRunner> {
        let mut runner = TestRunner {
            include: vec![],
            results: RegexTestResults::new(),
            expanders: vec![],
        };
        for mut substring in read_env(ENV_REGEX_TEST)?.split(",") {
            substring = substring.trim();
            if substring.is_empty() {
                continue;
            }
            if substring.starts_with("-") {
                runner.blacklist(&substring[1..]);
            } else {
                runner.whitelist(substring);
            }
        }
        Ok(runner)
    }

    /// Assert that all tests run have either passed or have been skipped.
    ///
    /// If any tests have failed, then a panic occurs with a report of all
    /// failures.
    ///
    /// If `REGEX_TEST_VERBOSE` is set to `1`, then a longer report of tests
    /// that passed, failed or skipped is printed.
    pub fn assert(&mut self) {
        self.results.assert();
    }

    /// Whitelist the given substring.
    ///
    /// Whitelist and blacklist rules are only applied when
    /// [`TestRunner::test_iter`] is called.
    pub fn whitelist(&mut self, substring: &str) -> &mut TestRunner {
        self.include.push(IncludePattern {
            blacklist: false,
            substring: BString::from(substring),
        });
        self
    }

    /// Whitelist the given iterator substrings.
    ///
    /// This is a convenience routine for calling `whitelist` on each of the
    /// substrings in the iterator provided.
    ///
    /// Whitelist and blacklist rules are only applied when
    /// [`TestRunner::test_iter`] is called.
    pub fn whitelist_iter<I, S>(&mut self, substrings: I) -> &mut TestRunner
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        for substring in substrings {
            self.whitelist(substring.as_ref());
        }
        self
    }

    /// Blacklist the given substring.
    ///
    /// A blacklisted test is never run, unless a whitelisted substring added
    /// after the blacklisted substring matches it.
    ///
    /// Whitelist and blacklist rules are only applied when
    /// [`TestRunner::test_iter`] is called.
    pub fn blacklist(&mut self, substring: &str) -> &mut TestRunner {
        self.include.push(IncludePattern {
            blacklist: true,
            substring: BString::from(substring),
        });
        self
    }

    /// Blacklist the given iterator substrings.
    ///
    /// A blacklisted test is never run, unless a whitelisted substring added
    /// after the blacklisted substring matches it.
    ///
    /// This is a convenience routine for calling `blacklist` on each of the
    /// substrings in the iterator provided.
    ///
    /// Whitelist and blacklist rules are only applied when
    /// [`TestRunner::test_iter`] is called.
    pub fn blacklist_iter<I, S>(&mut self, substrings: I) -> &mut TestRunner
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        for substring in substrings {
            self.blacklist(substring.as_ref());
        }
        self
    }

    /// Set an expansion predicate that appends each entry in
    /// `additional_names` to the end the name for every test that `predicate`
    /// returns true. Moreover, the corresponding additional name is made
    /// available via [`RegexTest::additional_name`].
    ///
    /// This permits implementors to create multiple copies of each test, and
    /// then do specifically different tasks with each, while making it so each
    /// test is distinct.
    ///
    /// For example, you might write something like this:
    ///
    /// ```ignore
    /// TestRunner::new()?
    ///     .expand(&["is_match", "find"], |t| t.compiles())
    ///     .test_iter(tests, compiler)
    ///     .assert()
    /// ```
    ///
    /// where each test that is expected to have a regex compile gets copied
    /// with `/is_match` and `/find` appends to the end of its name. Then, in
    /// your own test runner, you can inspect [`RegexTest::additional_name`] to
    /// decide what to do. In the case of `is_match`, you might test your regex
    /// engines "has a match" API, which might exercise different logic than
    /// your "find where the matches are" API.
    pub fn expand<S: AsRef<str>>(
        &mut self,
        additional_names: &[S],
        predicate: impl FnMut(&RegexTest) -> bool + 'static,
    ) -> &mut TestRunner {
        self.expanders.push(Expander {
            predicate: Box::new(predicate),
            additional_names: additional_names
                .iter()
                .map(|s| s.as_ref().to_string())
                .collect(),
        });
        self
    }

    /// Run all of the given tests using the given regex compiler.
    ///
    /// The compiler given is a closure that accepts a
    /// [`&RegexTest`](RegexTest) and a sequence of patterns, and returns (if
    /// successful) a [`CompiledRegex`] which can execute a search.
    ///
    /// Note that if there are test failures, this merely _collects_ them. Use
    /// [`TestRunner::assert`] to fail the current test by panicking if there
    /// any failures.
    ///
    /// Typically, one provides [`RegexTests::iter`] as the iterator of
    /// `RegexTest` values.
    pub fn test_iter<I, T>(
        &mut self,
        it: I,
        mut compile: impl FnMut(&RegexTest, &[String]) -> Result<CompiledRegex>,
    ) -> &mut TestRunner
    where
        I: IntoIterator<Item = T>,
        T: Borrow<RegexTest>,
    {
        for test in it {
            let test = test.borrow();
            let mut additional = vec![];
            for expander in &mut self.expanders {
                if (expander.predicate)(test) {
                    for name in expander.additional_names.iter() {
                        additional.push(test.with_additional_name(name));
                    }
                    break;
                }
            }
            if additional.is_empty() {
                additional.push(test.to_owned());
            }
            for test in &additional {
                if self.should_skip(test) {
                    self.results.skip(test);
                    continue;
                }
                self.test(test, |regexes| compile(test, regexes));
            }
        }
        self
    }

    /// Run a single test.
    ///
    /// This records the result of running the test in this runner. This does
    /// not fail the test immediately if the given regex test fails. Instead,
    /// this is only done when the `assert` method is called.
    ///
    /// Note that using this method bypasses any whitelist or blacklist applied
    /// to this runner. Whitelisted (and blacklisted) substrings are only
    /// applied when using `test_iter`.
    pub fn test(
        &mut self,
        test: &RegexTest,
        mut compile: impl FnMut(&[String]) -> Result<CompiledRegex>,
    ) -> &mut TestRunner {
        let mut compiled = match safe(|| compile(test.regexes())) {
            Err(msg) => {
                // Regex tests should never panic. It's auto-fail if they do.
                self.results.fail(
                    test,
                    RegexTestFailureKind::UnexpectedPanicCompile(msg),
                );
                return self;
            }
            Ok(Ok(compiled)) => compiled,
            Ok(Err(err)) => {
                if !test.compiles() {
                    self.results.pass(test);
                } else {
                    self.results.fail(
                        test,
                        RegexTestFailureKind::CompileError { err },
                    );
                }
                return self;
            }
        };
        // We fail the test if we didn't expect the regex to compile. However,
        // it's possible the caller decided to skip the test when attempting
        // to compile the regex, so we check for that. If the compiled regex
        // is marked as skipped, then 'test.test(..)' below handles it
        // correctly.
        if !test.compiles() && !compiled.is_skip() {
            self.results.fail(test, RegexTestFailureKind::NoCompileError);
            return self;
        }
        let result = match safe(|| test.test(&mut compiled)) {
            Ok(result) => result,
            Err(msg) => {
                self.results.fail(
                    test,
                    RegexTestFailureKind::UnexpectedPanicSearch(msg),
                );
                return self;
            }
        };
        match result.kind {
            TestResultKind::Match(yes) => {
                if yes == test.is_match() {
                    self.results.pass(test);
                } else {
                    self.results.fail(test, RegexTestFailureKind::IsMatch);
                }
            }
            TestResultKind::Which(which) => {
                if which != test.which_matches() {
                    self.results
                        .fail(test, RegexTestFailureKind::Many { got: which });
                } else {
                    self.results.pass(test);
                }
            }
            TestResultKind::StartEnd(matches) => {
                let expected = test.matches();
                if expected != matches {
                    self.results.fail(
                        test,
                        RegexTestFailureKind::StartEnd { got: matches },
                    );
                } else {
                    self.results.pass(test);
                }
            }
            TestResultKind::Captures(caps) => {
                let expected = test.captures();
                if expected != caps {
                    self.results.fail(
                        test,
                        RegexTestFailureKind::Captures { got: caps },
                    );
                } else {
                    self.results.pass(test);
                }
            }
            TestResultKind::Skip => {
                self.results.skip(test);
            }
            TestResultKind::Fail { why } => {
                self.results
                    .fail(test, RegexTestFailureKind::UserFailure { why });
            }
        }
        self
    }

    /// Return true if and only if the given test should be skipped.
    fn should_skip(&self, test: &RegexTest) -> bool {
        if self.include.is_empty() {
            return false;
        }

        // If we don't have any whitelist patterns, then the test will be run
        // unless it is blacklisted. Otherwise, if there are whitelist
        // patterns, then the test must match at least one of them.
        let mut skip = self.include.iter().any(|pat| !pat.blacklist);
        for pat in &self.include {
            if test.full_name().as_bytes().contains_str(&pat.substring) {
                skip = pat.blacklist;
            }
        }
        skip
    }
}

#[derive(Debug)]
struct IncludePattern {
    blacklist: bool,
    substring: BString,
}

struct Expander {
    predicate: Box<dyn FnMut(&RegexTest) -> bool>,
    additional_names: Vec<String>,
}

impl std::fmt::Debug for Expander {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Expander")
            .field("predicate", &"<FnMut(..)>")
            .field("additional_names", &self.additional_names)
            .finish()
    }
}

/// A collection of test results, corresponding to passed, skipped and failed
/// tests.
#[derive(Debug)]
struct RegexTestResults {
    pass: Vec<RegexTestResult>,
    fail: Vec<RegexTestFailure>,
    skip: Vec<RegexTestResult>,
}

/// A test that passed or skipped, along with its specific result.
#[derive(Debug)]
struct RegexTestResult {
    test: RegexTest,
}

/// A test that failed along with the reason why.
#[derive(Debug)]
struct RegexTestFailure {
    test: RegexTest,
    kind: RegexTestFailureKind,
}

/// Describes the nature of the failed test.
#[derive(Debug)]
enum RegexTestFailureKind {
    /// UserFailure indicates that the test failed because the test function
    /// explicitly failed it for the reason in the message given.
    UserFailure { why: String },
    /// This occurs when the test expected a match (or didn't expect a match),
    /// but the actual regex implementation didn't match (or did match).
    IsMatch,
    /// This occurs when a set of regexes is tested, and the matching regexes
    /// returned by the regex implementation don't match the expected matching
    /// regexes. This error contains the indices of the regexes that matched.
    Many { got: Vec<usize> },
    /// This occurs when a single regex is used to find all non-overlapping
    /// matches in a haystack, where the result did not match what was
    /// expected. This reports the incorrect matches returned by the regex
    /// implementation under test.
    StartEnd { got: Vec<Match> },
    /// Like StartEnd, but for capturing groups.
    Captures { got: Vec<Captures> },
    /// This occurs when the test expected the regex to fail to compile, but it
    /// compiled successfully.
    NoCompileError,
    /// This occurs when the test expected the regex to compile successfully,
    /// but it failed to compile.
    CompileError { err: anyhow::Error },
    /// While compiling, a panic occurred. If possible, the panic message
    /// is captured.
    UnexpectedPanicCompile(String),
    /// While searching, a panic occurred. If possible, the panic message
    /// is captured.
    UnexpectedPanicSearch(String),
}

impl RegexTestResults {
    fn new() -> RegexTestResults {
        RegexTestResults { pass: vec![], fail: vec![], skip: vec![] }
    }

    fn pass(&mut self, test: &RegexTest) {
        self.pass.push(RegexTestResult { test: test.clone() });
    }

    fn fail(&mut self, test: &RegexTest, kind: RegexTestFailureKind) {
        self.fail.push(RegexTestFailure { test: test.clone(), kind });
    }

    fn skip(&mut self, test: &RegexTest) {
        self.skip.push(RegexTestResult { test: test.clone() });
    }

    fn assert(&self) {
        if read_env(ENV_REGEX_TEST_VERBOSE).map_or(false, |s| s == "1") {
            self.verbose();
        }
        if self.fail.is_empty() {
            return;
        }
        let failures = self
            .fail
            .iter()
            .map(|f| f.to_string())
            .collect::<Vec<String>>()
            .join("\n\n");
        panic!(
            "found {} failures:\n{}\n{}\n{}\n\n\
             Set the REGEX_TEST environment variable to filter tests, \n\
             e.g., REGEX_TEST=foo,-foo2 runs every test whose name contains \n\
             foo but not foo2\n\n",
            self.fail.len(),
            "~".repeat(79),
            failures.trim(),
            "~".repeat(79),
        )
    }

    fn verbose(&self) {
        println!("{}", "~".repeat(79));
        for t in &self.skip {
            println!("skip: {}", t.full_name());
        }
        for t in &self.pass {
            println!("pass: {}", t.full_name());
        }
        for t in &self.fail {
            println!("FAIL: {}", t.test.full_name());
        }
        println!(
            "\npassed: {}, skipped: {}, failed: {}",
            self.pass.len(),
            self.skip.len(),
            self.fail.len()
        );
        println!("{}", "~".repeat(79));
    }
}

impl RegexTestResult {
    fn full_name(&self) -> String {
        self.test.full_name().to_string()
    }
}

impl RegexTestFailure {
    fn full_name(&self) -> String {
        self.test.full_name().to_string()
    }
}

impl std::fmt::Display for RegexTestFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}: {}\n\
             pattern:     {:?}\n\
             haystack:    {:?}",
            self.full_name(),
            self.kind.fmt(&self.test)?,
            self.test.regexes(),
            self.test.haystack().as_bstr(),
        )?;
        Ok(())
    }
}

impl RegexTestFailureKind {
    fn fmt(&self, test: &RegexTest) -> Result<String, std::fmt::Error> {
        use std::fmt::Write;

        let mut buf = String::new();
        match *self {
            RegexTestFailureKind::UserFailure { ref why } => {
                write!(buf, "failed by implementor because: {}", why)?;
            }
            RegexTestFailureKind::IsMatch => {
                if test.is_match() {
                    write!(buf, "expected match, but none found")?;
                } else {
                    write!(buf, "expected no match, but found a match")?;
                }
            }
            RegexTestFailureKind::Many { ref got } => {
                write!(
                    buf,
                    "expected regexes {:?} to match, but found {:?}",
                    test.which_matches(),
                    got
                )?;
            }
            RegexTestFailureKind::StartEnd { ref got } => {
                write!(
                    buf,
                    "did not find expected matches\n\
                     expected: {:?}\n     \
                     got: {:?}",
                    test.matches(),
                    got,
                )?;
            }
            RegexTestFailureKind::Captures { ref got } => {
                write!(
                    buf,
                    "expected to find {:?} captures, but got {:?}",
                    test.captures(),
                    got,
                )?;
            }
            RegexTestFailureKind::NoCompileError => {
                write!(buf, "expected regex to NOT compile, but it did")?;
            }
            RegexTestFailureKind::CompileError { ref err } => {
                write!(buf, "expected regex to compile, failed: {}", err)?;
            }
            RegexTestFailureKind::UnexpectedPanicCompile(ref msg) => {
                write!(buf, "got unexpected panic while compiling:\n{}", msg)?;
            }
            RegexTestFailureKind::UnexpectedPanicSearch(ref msg) => {
                write!(buf, "got unexpected panic while searching:\n{}", msg)?;
            }
        }
        Ok(buf)
    }
}

/// An iterator over regex tests.
///
/// This iterator is created by the [`RegexTests::iter`] method.
#[derive(Debug)]
pub struct RegexTestsIter<'a>(std::slice::Iter<'a, RegexTest>);

impl<'a> Iterator for RegexTestsIter<'a> {
    type Item = &'a RegexTest;

    fn next(&mut self) -> Option<&'a RegexTest> {
        self.0.next()
    }
}

/// Represents either a single regex or a list of regexes in a TOML.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(untagged)]
enum RegexesFormat {
    Single(String),
    Many(Vec<String>),
}

impl RegexesFormat {
    fn patterns(&self) -> &[String] {
        match *self {
            RegexesFormat::Single(ref pat) => std::slice::from_ref(pat),
            RegexesFormat::Many(ref pats) => pats,
        }
    }
}

/// Captures represents a single group of captured matches from a regex search.
///
/// There is always at least 1 group, and the first group is always present and
/// corresponds to the overall match.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(try_from = "CapturesFormat")]
pub struct Captures {
    /// The ID of the regex that matched.
    ///
    /// The ID is the index of the regex provided to the regex compiler,
    /// starting from `0`. In the case of a single regex search, the only
    /// possible ID is `0`.
    id: usize,
    /// The capturing groups that matched, along with the match offsets for
    /// each. The first group should always be non-None, as it corresponds to
    /// the overall match.
    ///
    /// This should either have length 1 (when not capturing group offsets are
    /// included in the tes tresult) or it should have length equal to the
    /// number of capturing groups in the regex pattern.
    groups: Vec<Option<Span>>,
}

impl Captures {
    /// Create a new set of captures for a single match of a regex.
    ///
    /// If available, iterator should provide items for every capturing group
    /// in the regex, including the 0th capturing group corresponding to the
    /// entire match. At minimum, the 0th capturing group should be provided.
    ///
    /// If a capturing group did not participate in the match, then a `None`
    /// value should be used. (The 0th capturing group should never be `None`.)
    ///
    /// If the iterator yields no elements or the first group is `None`, then
    /// this returns an error.
    ///
    /// The `id` should be the ID of the pattern that matched. This is always
    /// `0` for single-pattern regexes. Otherwise, the ID of a pattern starts
    /// at `0` and is incremented by 1 for each subsequent pattern.
    ///
    /// Note that there are possibly more convenient and infallible `From`
    /// impls for converting a `Match` or a `Span` into a `Captures`.
    pub fn new<I: IntoIterator<Item = Option<Span>>>(
        id: usize,
        it: I,
    ) -> Result<Captures> {
        let groups: Vec<Option<Span>> = it.into_iter().collect();
        if groups.is_empty() {
            bail!("captures must contain at least one group");
        } else if groups[0].is_none() {
            bail!("first group (index 0) of captures must be non-None");
        }
        Ok(Captures { id, groups })
    }

    /// Returns the ID of the pattern that matched.
    ///
    /// For any single pattern regexes, this should always be zero.
    pub fn id(&self) -> usize {
        self.id
    }

    /// Returns a slice of the underlying spans, each group corresponding to
    /// the (possibly) matched span. The first group in the slice returned
    /// is guaranteed to correspond to the overall match span and is thus
    /// non-`None`. All other groups may be `None`. Similarly, the slice is
    /// guaranteed to have length at least 1.
    pub fn groups(&self) -> &[Option<Span>] {
        &self.groups
    }

    /// Returns the number of groups (including the first) in these captures.
    ///
    /// The length returned is guaranteed to be greater than zero.
    pub fn len(&self) -> usize {
        self.groups.len()
    }

    /// Returns the overall match, including the pattern ID, for this group
    /// of captures.
    pub fn to_match(&self) -> Match {
        Match { id: self.id(), span: self.to_span() }
    }

    /// Returns the overall match span for this group of captures.
    pub fn to_span(&self) -> Span {
        // This is OK because a Captures value must always have at least one
        // group where the first group always corresponds to match offsets.
        self.groups[0].unwrap()
    }
}

/// Converts a plain `Match` to a `Captures` value, where the match corresponds
/// to the first and only group in `Captures`.
impl From<Match> for Captures {
    fn from(m: Match) -> Captures {
        Captures { id: m.id, groups: vec![Some(m.span)] }
    }
}

/// Converts a plain `Span` to a `Captures` value, where the span corresponds to
/// the first and only group in `Captures`. Since a `Span` does not contain a
/// pattern ID, the pattern ID used in this conversion is always `0`.
impl From<Span> for Captures {
    fn from(sp: Span) -> Captures {
        Captures { id: 0, groups: vec![Some(sp)] }
    }
}

/// Represents the actual 'captures' key format more faithfully such that
/// Serde can deserialize it.
///
/// Namely, we need a way to represent a 'None' value inside a TOML array, and
/// TOML has no 'null' value. So we make '[]' be 'None', and we use 'MaybeSpan'
/// to recognize it.
#[derive(Deserialize)]
#[serde(untagged)]
enum CapturesFormat {
    Span([usize; 2]),
    Match { id: usize, span: [usize; 2] },
    Spans(Vec<MaybeSpan>),
    Captures { id: usize, spans: Vec<MaybeSpan> },
}

impl TryFrom<CapturesFormat> for Captures {
    type Error = anyhow::Error;

    fn try_from(data: CapturesFormat) -> Result<Captures> {
        match data {
            CapturesFormat::Span([start, end]) => {
                Ok(Captures { id: 0, groups: vec![Some(Span { start, end })] })
            }
            CapturesFormat::Match { id, span: [start, end] } => {
                Ok(Captures { id, groups: vec![Some(Span { start, end })] })
            }
            CapturesFormat::Spans(spans) => {
                Captures::new(0, spans.into_iter().map(|s| s.into_option()))
            }
            CapturesFormat::Captures { id, spans } => {
                Captures::new(id, spans.into_iter().map(|s| s.into_option()))
            }
        }
    }
}

/// A single match, consisting of the pattern that matched and its span.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Match {
    /// The ID of the pattern that matched.
    ///
    /// This is always `0` for single-pattern regexes. Otherwise, patterns
    /// start at `0` and count upwards in increments of `1`.
    pub id: usize,
    /// The span of the overall match.
    pub span: Span,
}

impl std::fmt::Debug for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Match({:?}: {:?})", self.id, self.span)
    }
}

/// A span of contiguous bytes, from start to end, represented via byte
/// offsets.
///
/// The range is inclusive at the beginning and exclusive at the end.
#[derive(Clone, Copy, Deserialize, Eq, PartialEq)]
pub struct Span {
    /// The starting byte offset of the match.
    pub start: usize,
    /// The ending byte offset of the match.
    pub end: usize,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

/// Represents a single span, either present or empty.
///
/// An empty span is spelled `[]` in TOML, and a present span is spelled `[m,
/// n]`.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(untagged)]
enum MaybeSpan {
    None([usize; 0]),
    Some([usize; 2]),
}

impl MaybeSpan {
    /// Converts this TOML representation of a possibly absent span to a proper
    /// `Option<Span>`.
    fn into_option(self) -> Option<Span> {
        match self {
            MaybeSpan::None(_) => None,
            MaybeSpan::Some([start, end]) => Some(Span { start, end }),
        }
    }
}

/// The match semantics to use for a search.
///
/// When not specified in a test, the default is `MatchKind::LeftmostFirst`.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub enum MatchKind {
    /// All possible matches should be reported.
    ///
    /// Usually this makes it impossible for non-greedy repetition operators
    /// to exist. That is, they behave as greedy repetition operators.
    All,
    /// Report only the leftmost match. When there are multiple leftmost
    /// matches that start at the same position, prefer the one that comes
    /// "first" in the pattern. For example, `sam|samwise` matches `sam` in
    /// `samwise`.
    ///
    /// This typically corresponds to the semantics implemented by backtracking
    /// engines.
    LeftmostFirst,
    /// Report only the leftmost match. When there are multiple leftmost
    /// matches that start at the same position, prefer the one the longest
    /// match. For example, `sam|samwise` matches `samwise` in `samwise`.
    ///
    /// This typically corresponds to the semantics implemented by POSIX
    /// engines.
    LeftmostLongest,
}

impl Default for MatchKind {
    fn default() -> MatchKind {
        MatchKind::LeftmostFirst
    }
}

/// Represents the type of search to perform.
///
/// When not specified in a test, the default is `SearchKind::Leftmost`.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub enum SearchKind {
    /// Report matches as soon as they are found.
    ///
    /// This is somewhat tricky to test, as this semantic is specified in terms
    /// of whatever the regex engine can do. For example, an automata oriented
    /// engine might be able to report a match earlier than a backtracking
    /// engine.
    Earliest,
    /// A standard leftmost search, returning either the leftmost-first or
    /// leftmost-longest match. Generally speaking, it doesn't make sense to
    /// use this type of search in combination with [`MatchKind::All`].
    Leftmost,
    /// Return all possible matches, including ones that overlap. Typically
    /// this search kind is used in combination with [`MatchKind::All`].
    Overlapping,
}

impl Default for SearchKind {
    fn default() -> SearchKind {
        SearchKind::Leftmost
    }
}

/// Read the environment variable given. If it doesn't exist, then return an
/// empty string. Otherwise, check that it is valid UTF-8. If it isn't, return
/// a useful error message.
fn read_env(var: &str) -> Result<String> {
    let val = match std::env::var_os(var) {
        None => return Ok("".to_string()),
        Some(val) => val,
    };
    let val = val.into_string().map_err(|os| {
        anyhow::anyhow!(
            "invalid UTF-8 in env var {}={:?}",
            var,
            Vec::from_os_str_lossy(&os)
        )
    })?;
    Ok(val)
}

/// Runs the given closure such that any panics are caught and converted into
/// errors. If the panic'd value could not be converted to a known error type,
/// then a generic string error message is used.
///
/// This is useful for use inside the test runner such that bugs for certain
/// tests don't prevent other tests from running.
fn safe<T, F>(fun: F) -> Result<T, String>
where
    F: FnOnce() -> T,
{
    use std::panic;

    panic::catch_unwind(panic::AssertUnwindSafe(fun)).map_err(|any_err| {
        // Extract common types of panic payload:
        // panic and assert produce &str or String
        if let Some(&s) = any_err.downcast_ref::<&str>() {
            s.to_owned()
        } else if let Some(s) = any_err.downcast_ref::<String>() {
            s.to_owned()
        } else {
            "UNABLE TO SHOW RESULT OF PANIC.".to_owned()
        }
    })
}

/// A function to set some boolean fields to a default of 'true'. We use a
/// function so that we can hand a path to it to Serde.
fn default_true() -> bool {
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn err_no_regexes() {
        let data = r#"
[[test]]
name = "foo"
haystack = "lib.rs"
matches = true
case-insensitive = true
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }

    #[test]
    fn err_unknown_field() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = true
something = 0
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }

    #[test]
    fn err_no_matches() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }

    #[test]
    fn load_match() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [[0, 6]]
compiles = false
anchored = true
case-insensitive = true
unicode = false
utf8 = false
"#;

        let mut tests = RegexTests::new();
        tests.load_slice("test", data.as_bytes()).unwrap();

        let t0 = &tests.tests[0];
        assert_eq!("test", t0.group());
        assert_eq!("foo", t0.name());
        assert_eq!("test/foo", t0.full_name());
        assert_eq!(&[".*.rs"], t0.regexes());
        assert_eq!(true, t0.is_match());
        assert_eq!(vec![0], t0.which_matches());

        assert!(!t0.compiles());
        assert!(t0.anchored());
        assert!(t0.case_insensitive());
        assert!(!t0.unicode());
        assert!(!t0.utf8());
    }

    #[test]
    fn load_which_matches() {
        let data = r#"
[[test]]
name = "foo"
regex = [".*.rs", ".*.toml"]
haystack = "lib.rs"
matches = [
    { id = 0, spans = [[0, 0]] },
    { id = 2, spans = [[0, 0]] },
    { id = 5, spans = [[0, 0]] },
]
"#;

        let mut tests = RegexTests::new();
        tests.load_slice("test", data.as_bytes()).unwrap();

        let t0 = &tests.tests[0];
        assert_eq!(&[".*.rs", ".*.toml"], t0.regexes());
        assert_eq!(true, t0.is_match());
        assert_eq!(vec![0, 2, 5], t0.which_matches());

        assert!(t0.compiles());
        assert!(!t0.anchored());
        assert!(!t0.case_insensitive());
        assert!(t0.unicode());
        assert!(t0.utf8());
    }

    #[test]
    fn load_spans() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [[0, 2], [5, 10]]
"#;

        let mut tests = RegexTests::new();
        tests.load_slice("test", data.as_bytes()).unwrap();

        let spans =
            vec![Span { start: 0, end: 2 }, Span { start: 5, end: 10 }];
        let t0 = &tests.tests[0];
        assert_eq!(t0.regexes(), &[".*.rs"]);
        assert_eq!(t0.is_match(), true);
        assert_eq!(t0.which_matches(), &[0]);
        assert_eq!(
            t0.matches(),
            vec![
                Match { id: 0, span: spans[0] },
                Match { id: 0, span: spans[1] },
            ]
        );
        assert_eq!(
            t0.captures(),
            vec![
                Captures::new(0, vec![Some(spans[0])]).unwrap(),
                Captures::new(0, vec![Some(spans[1])]).unwrap(),
            ]
        );
    }

    #[test]
    fn load_capture_spans() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [
  [[0, 15], [5, 10], [], [13, 14]],
  [[20, 30], [22, 24], [25, 27], []],
]
"#;

        let mut tests = RegexTests::new();
        tests.load_slice("test", data.as_bytes()).unwrap();

        let t0 = &tests.tests[0];
        assert_eq!(t0.regexes(), &[".*.rs"]);
        assert_eq!(t0.is_match(), true);
        assert_eq!(t0.which_matches(), &[0]);
        assert_eq!(
            t0.matches(),
            vec![
                Match { id: 0, span: Span { start: 0, end: 15 } },
                Match { id: 0, span: Span { start: 20, end: 30 } },
            ]
        );
        assert_eq!(
            t0.captures(),
            vec![
                Captures::new(
                    0,
                    vec![
                        Some(Span { start: 0, end: 15 }),
                        Some(Span { start: 5, end: 10 }),
                        None,
                        Some(Span { start: 13, end: 14 }),
                    ]
                )
                .unwrap(),
                Captures::new(
                    0,
                    vec![
                        Some(Span { start: 20, end: 30 }),
                        Some(Span { start: 22, end: 24 }),
                        Some(Span { start: 25, end: 27 }),
                        None,
                    ]
                )
                .unwrap(),
            ]
        );
    }

    #[test]
    fn fail_spans_empty1() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [
  [],
]
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }

    #[test]
    fn fail_spans_empty2() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [
  [[]],
]
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }

    #[test]
    fn fail_spans_empty3() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [
  [[], [0, 2]],
]
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }

    #[test]
    fn fail_captures_empty1() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [
  { id = 0, spans = [] },
]
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }

    #[test]
    fn fail_captures_empty2() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [
  { id = 0, spans = [[]] },
]
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }

    #[test]
    fn fail_captures_empty3() {
        let data = r#"
[[test]]
name = "foo"
regex = ".*.rs"
haystack = "lib.rs"
matches = [
  { id = 0, spans = [[], [0, 2]] },
]
"#;

        let mut tests = RegexTests::new();
        assert!(tests.load_slice("test", data.as_bytes()).is_err());
    }
}
