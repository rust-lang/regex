use bstr::ByteVec;

use crate::args::Usage;

/// This defines a implementation for a flag that wants a single byte. This is
/// useful because there are some APIs that require a single byte. For example,
/// setting a line terminator.
///
/// This in particular supports the ability to write the byte via an escape
/// sequence. For example, `--flag '\xFF'` will parse to the single byte 0xFF.
///
/// If the flag value is empty or if it unescapes into something with more than
/// one byte, then it is considered an error.
#[derive(Debug, Default)]
pub struct OneByte(pub u8);

impl std::str::FromStr for OneByte {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<OneByte> {
        let bytes = Vec::unescape_bytes(s);
        anyhow::ensure!(
            bytes.len() == 1,
            "expected exactly one byte, but got {} bytes",
            bytes.len(),
        );
        Ok(OneByte(bytes[0]))
    }
}

/// This defines a implementation for a flag that wants a possibly empty set
/// of bytes. This is useful because there are some APIs that require multiple
/// individual bytes. For example, setting quit bytes for a DFA.
///
/// This in particular supports the ability to write the byte set via a
/// sequence of escape sequences. For example, `--flag 'a\xFF\t'` will parse to
/// the sequence 0x61 0xFF 0x09.
///
/// By default, the set is empty. If the flag value has a duplicate byte, then
/// an error is returned. An empty value corresponds to the empty set.
#[derive(Debug, Default)]
pub struct ByteSet(pub Vec<u8>);

impl std::str::FromStr for ByteSet {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<ByteSet> {
        let mut set = vec![];
        let mut seen = [false; 256];
        for &byte in Vec::unescape_bytes(s).iter() {
            anyhow::ensure!(
                !seen[usize::from(byte)],
                "saw duplicate byte 0x{:2X} in '{}'",
                byte,
                s,
            );
            seen[usize::from(byte)] = true;
            set.push(byte);
        }
        set.sort();
        Ok(ByteSet(set))
    }
}

/// Provides an implementation of the --start-kind flag, for use with DFA
/// configuration.
#[derive(Debug)]
pub struct StartKind {
    pub kind: regex_automata::dfa::StartKind,
}

impl StartKind {
    pub const USAGE: Usage = Usage::new(
        "--start-kind <kind>",
        "One of: both, unanchored, anchored.",
        r#"
Sets the start states supported by a DFA. The default is 'both', but it can
be set to either 'unanchored' or 'anchored'. The benefit of only supporting
unanchored or anchored start states is that it usually leads to a smaller
overall automaton.
"#,
    );
}

impl Default for StartKind {
    fn default() -> StartKind {
        StartKind { kind: regex_automata::dfa::StartKind::Both }
    }
}

impl std::str::FromStr for StartKind {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<StartKind> {
        let kind = match s {
            "both" => regex_automata::dfa::StartKind::Both,
            "unanchored" => regex_automata::dfa::StartKind::Unanchored,
            "anchored" => regex_automata::dfa::StartKind::Anchored,
            unk => anyhow::bail!("unrecognized start kind '{}'", unk),
        };
        Ok(StartKind { kind })
    }
}

/// Provides an implementation of the --match-kind flag, for use with most
/// regex matchers.
#[derive(Debug)]
pub struct MatchKind {
    pub kind: regex_automata::MatchKind,
}

impl MatchKind {
    pub const USAGE: Usage = Usage::new(
        "-k, --match-kind <kind>",
        "One of: leftmost-first, all.",
        r#"
Selects the match semantics for the regex engine. The choices are
'leftmost-first' (the default) or 'all'.

'leftmost-first' semantics look for the leftmost match, and when there are
multiple leftmost matches, match priority disambiguates them. For example,
in the haystack 'samwise', the regex 'samwise|sam' will match 'samwise' when
using leftmost-first semantics. Similarly, the regex 'sam|samwise' will match
'sam'.

'all' semantics results in including all possible match states in the
underlying automaton. When performing an unanchored leftmost search, this has
the effect of finding the last match, which is usually not what you want.
When performing an anchored leftmost search, it has the effect of finding the
longest possible match, which might be what you want. (So there is no support
for greedy vs non-greedy searching. Everything is greedy.) 'all' is also useful
for overlapping searches, since all matches are reportable in this scheme.
"#,
    );
}

impl Default for MatchKind {
    fn default() -> MatchKind {
        MatchKind { kind: regex_automata::MatchKind::LeftmostFirst }
    }
}

impl std::str::FromStr for MatchKind {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<MatchKind> {
        let kind = match s {
            "leftmost-first" => regex_automata::MatchKind::LeftmostFirst,
            "all" => regex_automata::MatchKind::All,
            unk => anyhow::bail!("unrecognized match kind '{}'", unk),
        };
        Ok(MatchKind { kind })
    }
}
