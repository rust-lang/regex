use std::cmp::Ordering;
use std::fmt;
use std::ops::Deref;
use std::mem;
use std::slice;

use char::Char;
use literals::{BuildPrefixes, Literals};

/// InstPtr represents the index of an instruction in a regex program.
pub type InstPtr = usize;

/// Insts is a sequence of instructions.
#[derive(Clone)]
pub struct Insts {
    insts: Vec<Inst>,
    bytes: bool,
    reverse: bool,
    byte_classes: Vec<u8>,
}

impl Insts {
    /// Create a new instruction sequence.
    ///
    /// If `bytes` is true, then this instruction sequence must run on raw
    /// bytes. Otherwise, it is executed on Unicode codepoints.
    ///
    /// A Vec<Inst> can be created with the compiler.
    pub fn new(
        insts: Vec<Inst>,
        bytes: bool,
        reverse: bool,
        byte_classes: Vec<u8>,
    ) -> Self {
        assert!(byte_classes.len() == 256);
        Insts {
            insts: insts,
            bytes: bytes,
            reverse: reverse,
            byte_classes: byte_classes,
        }
    }

    /// Returns true if and only if this instruction sequence must be executed
    /// on byte strings.
    pub fn is_bytes(&self) -> bool {
        self.bytes
    }

    /// Returns true if and only if this instruction sequence is reversed.
    pub fn is_reversed(&self) -> bool {
        self.reverse
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

    /// Returns a map from input byte to byte class. Each class represents
    /// a set of bytes that are indistinguishable to the underlying
    /// instructions.
    ///
    /// It is guaranteed to have length 256.
    pub fn byte_classes(&self) -> &[u8] {
        &self.byte_classes
    }

    /// Returns the location of the `Save(0)` instruction, which is present
    /// in every program and always indicates the logical start of a match.
    ///
    /// (DFA programs compile a `.*?` into the program, preceding the `Save(0)`
    /// instruction, to support unanchored matches. Generally, we want to
    /// ignore that `.*?` when doing analysis, like extracting prefixes.)
    pub fn start(&self) -> InstPtr {
        for (i, inst) in self.iter().enumerate() {
            match *inst {
                Inst::Save(ref inst) if inst.slot == 0 => return i,
                _ => {}
            }
        }
        unreachable!()
    }

    /// Return true if and only if an execution engine at instruction `pc` will
    /// always lead to a match.
    pub fn leads_to_match(&self, pc: usize) -> bool {
        match self[self.skip(pc)] {
            Inst::Match => true,
            _ => false,
        }
    }

    /// Return true if and only if the regex is anchored at the start of
    /// search text.
    pub fn anchored_begin(&self) -> bool {
        match self.get(1) {
            Some(&Inst::EmptyLook(ref inst)) => {
                inst.look == EmptyLook::StartText
            }
            _ => false,
        }
    }

    /// Return true if and only if the regex is anchored at the end of
    /// search text.
    pub fn anchored_end(&self) -> bool {
        match self.get(self.len() - 3) {
            Some(&Inst::EmptyLook(ref inst)) => {
                inst.look == EmptyLook::EndText
            }
            _ => false,
        }
    }

    /// Build a matching engine for all prefix literals in this instruction
    /// sequence.
    ///
    /// If there are no prefix literals (or there are too many), then a
    /// matching engine that never matches is returned.
    pub fn prefix_matcher(&self) -> Literals {
        if self.is_bytes() || self.is_reversed() {
            Literals::empty()
        } else {
            BuildPrefixes::new(self).literals().into_matcher()
        }
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

impl Deref for Insts {
    type Target = [Inst];

    fn deref(&self) -> &Self::Target {
        &*self.insts
    }
}

impl fmt::Debug for Insts {
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
                Match => try!(writeln!(f, "{:04} Match", pc)),
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

impl<'a> IntoIterator for &'a Insts {
    type Item = &'a Inst;
    type IntoIter = slice::Iter<'a, Inst>;
    fn into_iter(self) -> Self::IntoIter { self.iter() }
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
    Match,
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
            ref wbty => {
                let (w1, w2) = (c1.is_word_char(), c2.is_word_char());
                (*wbty == WordBoundary && w1 ^ w2)
                || (*wbty == NotWordBoundary && !(w1 ^ w2))
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
