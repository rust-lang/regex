use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::ops::Deref;
use std::slice;
use std::sync::Arc;

use input::Char;
use literal::LiteralSearcher;

/// `InstPtr` represents the index of an instruction in a regex program.
pub type InstPtr = usize;

/// Program is a sequence of instructions and various facts about thos
/// instructions.
#[derive(Clone)]
pub struct Program<I: InstTrait> {
    /// A sequence of instructions that represents an NFA.
    pub insts: Vec<I>,
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
    /// Whether this program contains a Unicode word boundary instruction.
    pub has_unicode_word_boundary: bool,
    /// A possibly empty machine for very quickly matching prefix literals.
    pub prefixes: LiteralSearcher,
    /// A limit on the size of the cache that the DFA is allowed to use while
    /// matching.
    ///
    /// The cache limit specifies approximately how much space we're willing to
    /// give to the state cache. Once the state cache exceeds the size, it is
    /// wiped and all states must be re-computed.
    ///
    /// Note that this value does not impact correctness. It can be set to 0
    /// and the DFA will run just fine. (It will only ever store exactly one
    /// state in the cache, and will likely run very slowly, but it will work.)
    ///
    /// Also note that this limit is *per thread of execution*. That is,
    /// if the same regex is used to search text across multiple threads
    /// simultaneously, then the DFA cache is not shared. Instead, copies are
    /// made.
    pub dfa_size_limit: usize,
}

impl<I: InstTrait> Program<I> {
    /// Creates an empty instruction sequence. Fields are given default
    /// values.
    pub fn new() -> Self {
        Program {
            insts: vec![],
            matches: vec![],
            captures: vec![],
            capture_name_idx: Arc::new(HashMap::new()),
            start: 0,
            byte_classes: vec![0; 256],
            only_utf8: true,
            is_dfa: false,
            is_reverse: false,
            is_anchored_start: false,
            is_anchored_end: false,
            has_unicode_word_boundary: false,
            prefixes: LiteralSearcher::empty(),
            dfa_size_limit: 2 * (1 << 20),
        }
    }

    /// If pc is an index to a no-op instruction (like Save), then return the
    /// next pc that is not a no-op instruction.
    pub fn skip(&self, mut pc: usize) -> usize {
        loop {
            match self[pc].save_goto() {
                Some(goto) => pc = goto,
                None => return pc,
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
        self[self.skip(pc)].is_match()
    }

    /// Returns true if the current configuration demands that an implicit
    /// `.*?` be prepended to the instruction sequence.
    pub fn needs_dotstar(&self) -> bool {
        self.is_dfa && !self.is_reverse && !self.is_anchored_start
    }

    /// Returns true if this program uses Byte instructions instead of
    /// Char/Range instructions.
    pub fn uses_bytes(&self) -> bool {
        I::IS_BYTES || self.is_dfa
    }

    /// Returns true if this program exclusively matches valid UTF-8 bytes.
    ///
    /// That is, if an invalid UTF-8 byte is seen, then no match is possible.
    pub fn only_utf8(&self) -> bool {
        self.only_utf8
    }

    /// Return the approximate heap usage of this instruction sequence in
    /// bytes.
    pub fn approximate_size(&self) -> usize {
        // The only instruction that uses heap space is Ranges (for
        // Unicode codepoint programs) to store non-overlapping codepoint
        // ranges. To keep this operation constant time, we ignore them.
        (self.len() * mem::size_of::<I>())
            + (self.matches.len() * mem::size_of::<InstPtr>())
            + (self.captures.len() * mem::size_of::<Option<String>>())
            + (self.capture_name_idx.len()
                * (mem::size_of::<String>() + mem::size_of::<usize>()))
            + (self.byte_classes.len() * mem::size_of::<u8>())
            + self.prefixes.approximate_size()
    }
}

impl<I: InstTrait> Deref for Program<I> {
    type Target = [I];

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn deref(&self) -> &Self::Target {
        &*self.insts
    }
}

impl<I: InstTrait> fmt::Debug for Program<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (pc, inst) in self.iter().enumerate() {
            write!(f, "{:04} {:?}", pc, inst)?;
            if let Some(goto) = inst.goto() {
                if pc + 1 == goto {
                    write!(f, " (goto: {})", goto)?;
                }
            }
            if pc == self.start {
                write!(f, " (start)")?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl<'a, I: InstTrait> IntoIterator for &'a Program<I> {
    type Item = &'a I;
    type IntoIter = slice::Iter<'a, I>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// `InstTrait` represents an instruction code in a Regex program.
///
/// Regrettably, a regex program either contains Unicode codepoint
/// instructions (Char and Ranges: [`UnicodeInst`]) or it contains
/// byte instructions (Bytes: [`BytesInst`]).
/// A regex program can never contain both.
pub trait InstTrait: fmt::Debug {
    const IS_BYTES: bool;

    /// Returns true if and only if this is a match instruction.
    fn is_match(&self) -> bool;
    fn goto(&self) -> Option<usize>;
    fn save_goto(&self) -> Option<usize>;
    fn new_match(i: usize) -> Self;
    fn new_split(split: InstSplit) -> Self;
}

/// A Unicode codepoint instruction.
#[derive(Clone)]
pub enum UnicodeInst {
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
}

impl InstTrait for UnicodeInst {
    const IS_BYTES: bool = false;

    #[inline]
    fn is_match(&self) -> bool {
        match *self {
            Self::Match(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn goto(&self) -> Option<usize> {
        match self {
            Self::Match(_) => None,
            Self::Save(ref inst) => Some(inst.goto),
            Self::Split(_) => None,
            Self::EmptyLook(ref inst) => Some(inst.goto),
            Self::Char(ref inst) => Some(inst.goto),
            Self::Ranges(ref inst) => Some(inst.goto),
        }
    }

    #[inline]
    fn save_goto(&self) -> Option<usize> {
        match self {
            Self::Save(ref inst) => Some(inst.goto),
            _ => None,
        }
    }

    #[inline]
    fn new_match(i: usize) -> Self {
        Self::Match(i)
    }

    #[inline]
    fn new_split(split: InstSplit) -> Self {
        Self::Split(split)
    }
}

impl fmt::Debug for UnicodeInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Match(slot) => write!(f, "Match({:?})", slot),
            Self::Save(ref inst) => write!(f, "Save({})", inst.slot),
            Self::Split(ref inst) => {
                write!(f, "Split({}, {})", inst.goto1, inst.goto2)
            }
            Self::EmptyLook(ref inst) => {
                write!(f, "{:?}", inst.look)
            }
            Self::Char(ref inst) => {
                write!(f, "{:?}", inst.c)
            }
            Self::Ranges(ref inst) => {
                write!(
                    f,
                    "{}",
                    inst.ranges
                        .iter()
                        .map(|r| format!("{:?}-{:?}", r.0, r.1))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

/// A byte instruction.
#[derive(Clone)]
pub enum BytesInst {
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
    /// Bytes is like Ranges, except it expresses a single byte range. It is
    /// used in conjunction with Split instructions to implement multi-byte
    /// character classes.
    Bytes(InstBytes),
}

impl InstTrait for BytesInst {
    const IS_BYTES: bool = true;

    #[inline]
    fn is_match(&self) -> bool {
        match *self {
            Self::Match(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn goto(&self) -> Option<usize> {
        match self {
            Self::Match(_) => None,
            Self::Save(ref inst) => Some(inst.goto),
            Self::Split(_) => None,
            Self::EmptyLook(ref inst) => Some(inst.goto),
            Self::Bytes(ref inst) => Some(inst.goto),
        }
    }

    #[inline]
    fn save_goto(&self) -> Option<usize> {
        match self {
            Self::Save(ref inst) => Some(inst.goto),
            _ => None,
        }
    }

    #[inline]
    fn new_match(i: usize) -> Self {
        Self::Match(i)
    }

    #[inline]
    fn new_split(split: InstSplit) -> Self {
        Self::Split(split)
    }
}

impl fmt::Debug for BytesInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn visible_byte(b: u8) -> String {
            use std::ascii::escape_default;
            let escaped = escape_default(b).collect::<Vec<u8>>();
            String::from_utf8_lossy(&escaped).into_owned()
        }

        match self {
            Self::Match(slot) => write!(f, "Match({:?})", slot),
            Self::Save(ref inst) => write!(f, "Save({})", inst.slot),
            Self::Split(ref inst) => {
                write!(f, "Split({}, {})", inst.goto1, inst.goto2)
            }
            Self::EmptyLook(ref inst) => {
                write!(f, "{:?}", inst.look)
            }
            Self::Bytes(ref inst) => {
                write!(
                    f,
                    "Bytes({}, {})",
                    visible_byte(inst.start),
                    visible_byte(inst.end)
                )
            }
        }
    }
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

/// Representation of the `EmptyLook` instruction.
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
        self.ranges
            .binary_search_by(|r| {
                if r.1 < c {
                    Ordering::Less
                } else if r.0 > c {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .is_ok()
    }

    /// Return the number of distinct characters represented by all of the
    /// ranges.
    pub fn num_chars(&self) -> usize {
        self.ranges
            .iter()
            .map(|&(s, e)| 1 + (e as u32) - (s as u32))
            .sum::<u32>() as usize
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

#[cfg(test)]
mod test {
    #[test]
    #[cfg(target_pointer_width = "64")]
    fn test_size_of_inst() {
        use std::mem::size_of;

        use super::{BytesInst, UnicodeInst};

        assert_eq!(24, size_of::<BytesInst>());
        assert_eq!(40, size_of::<UnicodeInst>());
    }
}
