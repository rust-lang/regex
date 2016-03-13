use std::collections::HashMap;
use std::cmp::Ordering;
use std::fmt;
use std::ops::Deref;
use std::mem;
use std::slice;
use std::sync::Arc;

use backtrack::BacktrackCache;
use dfa::DfaCache;
use input::Char;
use literals::Literals;
use nfa::NfaCache;
use pool::{Pool, PoolGuard};

/// InstPtr represents the index of an instruction in a regex program.
pub type InstPtr = usize;

/// Program is a sequence of instructions and various facts about thos
/// instructions.
#[derive(Clone)]
pub struct Program {
    /// A sequence of instructions that represents an NFA.
    pub insts: Vec<Inst>,
    /// Pointers to each Match instruction in the sequence.
    ///
    /// This is always length 1 unless this program represents a regex set.
    pub matches: Vec<InstPtr>,
    /// The ordered sequence of all capture groups extracted from the AST.
    /// Unnamed groups are `None`.
    pub captures: Vec<Option<String>>,
    /// Pointers to all named capture groups into `captures`.
    pub capture_name_idx: Arc<HashMap<String, usize>>,
    /// A pointer to the start instruction. This can vary depending on how
    /// the program was compiled. For example, programs for use with the DFA
    /// engine have a `.*?` inserted at the beginning of unanchored regular
    /// expressions. The actual starting point of the program is after the
    /// `.*?`.
    pub start: InstPtr,
    /// A set of equivalence classes for discriminating bytes in the compiled
    /// program.
    pub byte_classes: Vec<u8>,
    /// When true, this program can only match valid UTF-8.
    pub only_utf8: bool,
    /// When true, this program uses byte range instructions instead of Unicode
    /// range instructions.
    pub is_bytes: bool,
    /// When true, the program is compiled for DFA matching. For example, this
    /// implies `is_bytes` and also inserts a preceding `.*?` for unanchored
    /// regexes.
    pub is_dfa: bool,
    /// When true, the program matches text in reverse (for use only in the
    /// DFA).
    pub is_reverse: bool,
    /// Whether the regex must match from the start of the input.
    pub is_anchored_start: bool,
    /// Whether the regex must match at the end of the input.
    pub is_anchored_end: bool,
    /// A possibly empty machine for very quickly matching prefix literals.
    pub prefixes: Literals,
    /// Caches for use by the matching engines.
    pub cache: EngineCache,
}

impl Program {
    /// Creates an empty instruction sequence. Fields are given default
    /// values.
    pub fn new() -> Self {
        Program {
            insts: vec![],
            matches: vec![],
            captures: vec![],
            capture_name_idx: Arc::new(HashMap::new()),
            start: 0,
            byte_classes: vec![],
            only_utf8: true,
            is_bytes: false,
            is_dfa: false,
            is_reverse: false,
            is_anchored_start: false,
            is_anchored_end: false,
            prefixes: Literals::empty(),
            cache: EngineCache::new(),
        }
    }

    /// If pc is an index to a no-op instruction (like Save), then return the
    /// next pc that is not a no-op instruction.
    pub fn skip(&self, mut pc: usize) -> usize {
        loop {
            match self[pc] {
                Inst::Save(ref i) => pc = i.goto,
                _ => return pc,
            }
        }
    }

    /// Return true if and only if an execution engine at instruction `pc` will
    /// always lead to a match.
    pub fn leads_to_match(&self, pc: usize) -> bool {
        if self.matches.len() > 1 {
            // If we have a regex set, then we have more than one ending
            // state, so leading to one of those states is generally
            // meaningless.
            return false;
        }
        match self[self.skip(pc)] {
            Inst::Match(_) => true,
            _ => false,
        }
    }

    /// Returns true if the current configuration demands that an implicit
    /// `.*?` be prepended to the instruction sequence.
    pub fn needs_dotstar(&self) -> bool {
        self.is_dfa && !self.is_reverse && !self.is_anchored_start
    }

    /// Returns true if this program uses Byte instructions instead of
    /// Char/Range instructions.
    pub fn uses_bytes(&self) -> bool {
        self.is_bytes || self.is_dfa || !self.only_utf8
    }

    /// Returns true if this program exclusively matches valid UTF-8 bytes.
    ///
    /// That is, if an invalid UTF-8 byte is seen, then no match is possible.
    pub fn only_utf8(&self) -> bool {
        self.only_utf8
    }

    /// Retrieve cached state for NFA execution.
    pub fn cache_nfa(&self) -> PoolGuard<Box<NfaCache>> {
        self.cache.nfa.get()
    }

    /// Retrieve cached state for backtracking execution.
    pub fn cache_backtrack(&self) -> PoolGuard<Box<BacktrackCache>> {
        self.cache.backtrack.get()
    }

    /// Retrieve cached state for DFA execution.
    pub fn cache_dfa(&self) -> PoolGuard<Box<DfaCache>> {
        self.cache.dfa.get()
    }

    /// Return the approximate heap usage of this instruction sequence in
    /// bytes.
    pub fn approximate_size(&self) -> usize {
        // The only instruction that uses heap space is Ranges (for
        // Unicode codepoint programs) to store non-overlapping codepoint
        // ranges. To keep this operation constant time, we ignore them.
        self.len() * mem::size_of::<Inst>()
    }
}

impl Deref for Program {
    type Target = [Inst];

    fn deref(&self) -> &Self::Target {
        &*self.insts
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Inst::*;

        fn with_goto(cur: usize, goto: usize, fmtd: String) -> String {
            if goto == cur + 1 {
                fmtd
            } else {
                format!("{} (goto: {})", fmtd, goto)
            }
        }

        fn visible_byte(b: u8) -> String {
            use std::ascii::escape_default;
            let escaped = escape_default(b).collect::<Vec<u8>>();
            String::from_utf8_lossy(&escaped).into_owned()
        }

        try!(writeln!(f, "--------------------------------"));
        for (pc, inst) in self.iter().enumerate() {
            match *inst {
                Match(slot) => {
                    try!(writeln!(f, "{:04} Match({:?})", pc, slot))
                }
                Save(ref inst) => {
                    let s = format!("{:04} Save({})", pc, inst.slot);
                    try!(writeln!(f, "{}", with_goto(pc, inst.goto, s)));
                }
                Split(ref inst) => {
                    try!(writeln!(f, "{:04} Split({}, {})",
                                  pc, inst.goto1, inst.goto2));
                }
                EmptyLook(ref inst) => {
                    let s = format!("{:?}", inst.look);
                    try!(writeln!(f, "{:04} {}",
                                  pc, with_goto(pc, inst.goto, s)));
                }
                Char(ref inst) => {
                    let s = format!("{:?}", inst.c);
                    try!(writeln!(f, "{:04} {}",
                                  pc, with_goto(pc, inst.goto, s)));
                }
                Ranges(ref inst) => {
                    let ranges = inst.ranges
                        .iter()
                        .map(|r| format!("{:?}-{:?}", r.0, r.1))
                        .collect::<Vec<String>>()
                        .join(", ");
                    let s = format!("{}", ranges);
                    try!(writeln!(f, "{:04} {}",
                                  pc, with_goto(pc, inst.goto, s)));
                }
                Bytes(ref inst) => {
                    let s = format!(
                        "Bytes({}, {})",
                        visible_byte(inst.start),
                        visible_byte(inst.end));
                    try!(writeln!(f, "{:04} {}",
                                  pc, with_goto(pc, inst.goto, s)));
                }
            }
        }
        try!(writeln!(f, "--------------------------------"));
        Ok(())
    }
}

impl<'a> IntoIterator for &'a Program {
    type Item = &'a Inst;
    type IntoIter = slice::Iter<'a, Inst>;
    fn into_iter(self) -> Self::IntoIter { self.iter() }
}

/// EngineCache maintains reusable allocations for each matching engine
/// available to a particular program.
///
/// The allocations are created lazily, so we don't pay for caches that
/// aren't used.
///
/// N.B. These are all behind a pointer because it's fewer bytes to memcpy.
/// These caches are pushed/popped from the pool a lot, and a smaller
/// footprint can have an impact on matching small inputs. See, for example,
/// the hard_32 benchmark.
#[derive(Debug)]
pub struct EngineCache {
    nfa: Pool<Box<NfaCache>>,
    backtrack: Pool<Box<BacktrackCache>>,
    dfa: Pool<Box<DfaCache>>,
}

impl EngineCache {
    fn new() -> Self {
        EngineCache {
            nfa: Pool::new(Box::new(|| Box::new(NfaCache::new()))),
            backtrack: Pool::new(Box::new(|| Box::new(BacktrackCache::new()))),
            dfa: Pool::new(Box::new(|| Box::new(DfaCache::new()))),
        }
    }
}

impl Clone for EngineCache {
    fn clone(&self) -> EngineCache {
        EngineCache::new()
    }
}

/// Inst is an instruction code in a Regex program.
///
/// Regrettably, a regex program either contains Unicode codepoint
/// instructions (Char and Ranges) or it contains byte instructions (Bytes).
/// A regex program can never contain both.
///
/// It would be worth investigating splitting this into two distinct types and
/// then figuring out how to make the matching engines polymorphic over those
/// types without sacrificing performance.
///
/// Other than the benefit of moving invariants into the type system, another
/// benefit is the decreased size. If we remove the `Char` and `Ranges`
/// instructions from the `Inst` enum, then its size shrinks from 40 bytes to
/// 24 bytes. (This is because of the removal of a `Vec` in the `Ranges`
/// variant.) Given that byte based machines are typically much bigger than
/// their Unicode analogues (because they can decode UTF-8 directly), this ends
/// up being a pretty significant savings.
#[derive(Clone, Debug)]
pub enum Inst {
    /// Match indicates that the program has reached a match state.
    ///
    /// The number in the match corresponds to the Nth logical regular
    /// expression in this program. This index is always 0 for normal regex
    /// programs. Values greater than 0 appear when compiling regex sets, and
    /// each match instruction gets its own unique value. The value corresponds
    /// to the Nth regex in the set.
    Match(usize),
    /// Save causes the program to save the current location of the input in
    /// the slot indicated by InstSave.
    Save(InstSave),
    /// Split causes the program to diverge to one of two paths in the
    /// program, preferring goto1 in InstSplit.
    Split(InstSplit),
    /// EmptyLook represents a zero-width assertion in a regex program. A
    /// zero-width assertion does not consume any of the input text.
    EmptyLook(InstEmptyLook),
    /// Char requires the regex program to match the character in InstChar at
    /// the current position in the input.
    Char(InstChar),
    /// Ranges requires the regex program to match the character at the current
    /// position in the input with one of the ranges specified in InstRanges.
    Ranges(InstRanges),
    /// Bytes is like Ranges, except it expresses a single byte range. It is
    /// used in conjunction with Split instructions to implement multi-byte
    /// character classes.
    Bytes(InstBytes),
}

/// Representation of the Save instruction.
#[derive(Clone, Debug)]
pub struct InstSave {
    /// The next location to execute in the program.
    pub goto: InstPtr,
    /// The capture slot (there are two slots for every capture in a regex,
    /// including the zeroth capture for the entire match).
    pub slot: usize,
}

/// Representation of the Split instruction.
#[derive(Clone, Debug)]
pub struct InstSplit {
    /// The first instruction to try. A match resulting from following goto1
    /// has precedence over a match resulting from following goto2.
    pub goto1: InstPtr,
    /// The second instruction to try. A match resulting from following goto1
    /// has precedence over a match resulting from following goto2.
    pub goto2: InstPtr,
}

/// Representation of the EmptyLook instruction.
#[derive(Clone, Debug)]
pub struct InstEmptyLook {
    /// The next location to execute in the program if this instruction
    /// succeeds.
    pub goto: InstPtr,
    /// The type of zero-width assertion to check.
    pub look: EmptyLook,
}

/// The set of zero-width match instructions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmptyLook {
    /// Start of line or input.
    StartLine,
    /// End of line or input.
    EndLine,
    /// Start of input.
    StartText,
    /// End of input.
    EndText,
    /// Word character on one side and non-word character on other.
    WordBoundary,
    /// Word character on both sides or non-word character on both sides.
    NotWordBoundary,
    /// ASCII word boundary.
    WordBoundaryAscii,
    /// Not ASCII word boundary.
    NotWordBoundaryAscii,
}

impl InstEmptyLook {
    /// Tests whether the pair of characters matches this zero-width
    /// instruction.
    pub fn matches(&self, c1: Char, c2: Char) -> bool {
        use self::EmptyLook::*;
        match self.look {
            StartLine => c1.is_none() || c1 == '\n',
            EndLine => c2.is_none() || c2 == '\n',
            StartText => c1.is_none(),
            EndText => c2.is_none(),
            WordBoundary => {
                let (w1, w2) = (c1.is_word_char(), c2.is_word_char());
                w1 ^ w2
            }
            NotWordBoundary => {
                let (w1, w2) = (c1.is_word_char(), c2.is_word_char());
                !(w1 ^ w2)
            }
            WordBoundaryAscii => {
                let (w1, w2) = (c1.is_word_byte(), c2.is_word_byte());
                w1 ^ w2
            }
            NotWordBoundaryAscii => {
                let (w1, w2) = (c1.is_word_byte(), c2.is_word_byte());
                !(w1 ^ w2)
            }
        }
    }
}

/// Representation of the Char instruction.
#[derive(Clone, Debug)]
pub struct InstChar {
    /// The next location to execute in the program if this instruction
    /// succeeds.
    pub goto: InstPtr,
    /// The character to test.
    pub c: char,
}

/// Representation of the Ranges instruction.
#[derive(Clone, Debug)]
pub struct InstRanges {
    /// The next location to execute in the program if this instruction
    /// succeeds.
    pub goto: InstPtr,
    /// The set of Unicode scalar value ranges to test.
    pub ranges: Vec<(char, char)>,
}

impl InstRanges {
    /// Tests whether the given input character matches this instruction.
    #[inline(always)] // About ~5-15% more throughput then `#[inline]`
    pub fn matches(&self, c: Char) -> bool {
        // This speeds up the `match_class_unicode` benchmark by checking
        // some common cases quickly without binary search. e.g., Matching
        // a Unicode class on predominantly ASCII text.
        for r in self.ranges.iter().take(4) {
            if c < r.0 {
                return false;
            }
            if c <= r.1 {
                return true;
            }
        }
        self.ranges.binary_search_by(|r| {
            if r.1 < c {
                Ordering::Less
            } else if r.0 > c {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }).is_ok()
    }

    /// Return the number of distinct characters represented by all of the
    /// ranges.
    pub fn num_chars(&self) -> usize {
        self.ranges.iter()
            .map(|&(s, e)| 1 + (e as u32) - (s as u32))
            .fold(0, |acc, len| acc + len)
            as usize
    }
}

/// Representation of the Bytes instruction.
#[derive(Clone, Debug)]
pub struct InstBytes {
    /// The next location to execute in the program if this instruction
    /// succeeds.
    pub goto: InstPtr,
    /// The start (inclusive) of this byte range.
    pub start: u8,
    /// The end (inclusive) of this byte range.
    pub end: u8,
}

impl InstBytes {
    /// Returns true if and only if the given byte is in this range.
    pub fn matches(&self, byte: u8) -> bool {
        self.start <= byte && byte <= self.end
    }
}
