// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::char;
use std::collections::HashSet;
use std::fmt;
use std::mem;

use aho_corasick::{Automaton, AcAutomaton, FullAcAutomaton};
use memchr::{memchr, memchr2, memchr3};

use utf8::encode_utf8;
use prog::{Program, Inst, InstBytes, InstRanges};

#[derive(Clone, Eq, PartialEq)]
pub struct AlternateLiterals {
    at_match: bool,
    literals: Vec<Vec<u8>>,
}

impl AlternateLiterals {
    pub fn into_matcher(self) -> Literals {
        if self.literals.is_empty() {
            Literals::empty()
        } else {
            let alts = self.unambiguous();
            let at_match = alts.at_match;
            Literals {
                at_match: at_match,
                matcher: LiteralMatcher::new(alts),
            }
        }
    }

    fn empty() -> AlternateLiterals {
        AlternateLiterals { at_match: false, literals: vec![] }
    }

    fn is_empty(&self) -> bool {
        self.literals.is_empty()
    }

    fn is_single_byte(&self) -> bool {
        self.literals.len() == 1 && self.literals[0].len() == 1
    }

    fn all_single_bytes(&self) -> bool {
        self.literals.len() >= 1 && self.literals.iter().all(|s| s.len() == 1)
    }

    fn all_same_length(&self) -> bool {
        if self.literals.is_empty() {
            return true;
        }
        self.literals.iter().all(|s| s.len() == self.literals[0].len())
    }

    fn is_one_literal(&self) -> bool {
        self.literals.len() == 1
    }

    fn distinct_single_bytes(&self) -> usize {
        let mut seen = vec![false; 256];
        for lit in &self.literals {
            if lit.len() == 1 {
                seen[lit[0] as usize] = true;
            }
        }
        seen.iter().filter(|yes| **yes).count()
    }

    fn num_bytes(&self) -> usize {
        self.literals.iter().map(|lit| lit.len()).fold(0, |acc, len| acc + len)
    }

    fn add_alternates(&mut self, alts: AlternateLiterals) {
        self.at_match = self.at_match && alts.at_match;
        self.literals.extend(alts.literals);
    }

    fn add_literal_char(&mut self, c: char) {
        let scratch = &mut [0; 4];
        let n = encode_utf8(c, scratch).unwrap();
        for alt in &mut self.literals {
            alt.extend(&scratch[0..n]);
        }
    }

    fn add_literal_char_ranges(&mut self, inst: &InstRanges) {
        // This is tricky. We need to think of each range as its own set of
        // alternations. For example, `[a-cx-z]` is comprised of two ranges:
        // `a-c` and `x-z`. This is equivalent to the regex `a|b|c|x|y|z`. If
        // we've already found two prefixes, e.g., `foo|bar`, then we need to
        // extend all such prefixes with all alternates here. For e.g., `fooa`,
        // ..., `fooz`, `bara`, ..., `barz`.
        //
        // To achieve this, we copy our existing literals for every char!
        let scratch = &mut [0; 4];
        let nlits = self.literals.len();
        let orig = mem::replace(&mut self.literals, Vec::with_capacity(nlits));
        for &(s, e) in &inst.ranges {
            for c in (s as u32)..(e as u32 + 1) {
                for alt in &orig {
                    let mut alt = alt.clone();
                    let ch = char::from_u32(c).unwrap();
                    let n = encode_utf8(ch, scratch).unwrap();

                    alt.extend(&scratch[0..n]);
                    self.literals.push(alt);
                }
            }
        }
    }

    fn add_literal_byte_range(&mut self, inst: &InstBytes) {
        // Pretty much the same process as for literal char ranges, but we
        // only have one range.
        let nlits = self.literals.len();
        let orig = mem::replace(&mut self.literals, Vec::with_capacity(nlits));
        for b in inst.start..(inst.end + 1) {
            for alt in &orig {
                let mut alt = alt.clone();
                alt.push(b);
                self.literals.push(alt);
            }
        }
    }

    /// Returns a new set of alternate literals that are guaranteed to be
    /// unambiguous.
    ///
    /// State differently, the set of literals returned is guaranteed to never
    /// result in any overlapping matches.
    ///
    /// Duplicate literals are dropped. Literals that are otherwise distinct
    /// will be possibly truncated.
    fn unambiguous(&self) -> AlternateLiterals {
        fn position(needle: &[u8], mut haystack: &[u8]) -> Option<usize> {
            let mut i = 0;
            while haystack.len() >= needle.len() {
                if needle == &haystack[..needle.len()] {
                    return Some(i);
                }
                i += 1;
                haystack = &haystack[1..];
            }
            None
        }

        // This function is a bit gratuitous and allocation heavy, but in
        // general, we limit the number of alternates to be pretty small.

        if self.all_same_length() {
            return self.clone();
        }
        let mut new = AlternateLiterals {
            at_match: self.at_match,
            literals: Vec::with_capacity(self.literals.len()),
        };
'OUTER:
        for lit1 in &self.literals {
            if new.literals.is_empty() {
                new.literals.push(lit1.clone());
                continue;
            }
            let mut candidate = lit1.clone();
            for lit2 in &mut new.literals {
                if &candidate == lit2 {
                    // If the literal is already in the set, then we can
                    // just drop it.
                    continue 'OUTER;
                }
                if lit1.len() <= lit2.len() {
                    if let Some(i) = position(&candidate, lit2) {
                        new.at_match = false;
                        lit2.truncate(i);
                    }
                } else {
                    if let Some(i) = position(lit2, &candidate) {
                        new.at_match = false;
                        candidate.truncate(i);
                    }
                }
            }
            new.literals.push(candidate);
        }
        new.literals.retain(|lit| !lit.is_empty());
        // This is OK only because the alternates are unambiguous.
        new.literals.sort();
        new.literals.dedup();
        new
    }
}

struct BuildPrefixes<'a> {
    insts: &'a Program,
    limit: usize,
    alts: AlternateLiterals,
}

impl<'a> BuildPrefixes<'a> {
    fn new(insts: &'a Program) -> Self {
        BuildPrefixes {
            insts: insts,
            limit: 250,
            alts: AlternateLiterals { at_match: true, literals: vec![] },
        }
    }

    fn literals(mut self) -> AlternateLiterals {
        let mut stack = vec![self.insts.skip(self.insts.start)];
        let mut seen = HashSet::new();
        while let Some(mut pc) = stack.pop() {
            seen.insert(pc);
            pc = self.insts.skip(pc);
            if let Inst::Split(ref inst) = self.insts[pc] {
                if !seen.contains(&inst.goto2) {
                    stack.push(inst.goto2);
                }
                if !seen.contains(&inst.goto1) {
                    stack.push(inst.goto1);
                }
                continue;
            }
            // When searching for required literals, set the local limit to
            // something a bit less than our real limit. This prevents a single
            // alternation from blowing our budget in most cases. (If a single
            // alt blows the budget, then we can't consume literals from other
            // alts, which means we end up with nothing to show for it.)
            //
            // For example, consider `a?[0-9]{3}`. This splits into two
            // regexes `a[0-9]{3}` and `[0-9]{3}`. The latter regex can be
            // expanded completely into a set of alternate literals that
            // consumes exactly 3000 bytes. This is our entire budget if the
            // limit is 3000. Therefore, we're left with no room to add the
            // second branch (`a[0-9]{3}`) to our set of literals. If we can't
            // represent all required alternates, then we have to give up.
            // Therefore, as a heuristic, limit what each alternate is allowed
            // to use. In this case, `[0-9]{3}` will only gather literals for
            // `[0-9]{2}`, which leaves more than enough room for our second
            // branch.
            let alts = BuildRequiredLiterals::new(self.insts)
                                             .set_limit(self.limit / 10)
                                             .literals(pc);
            if alts.is_empty() {
                // If we couldn't find any literals required in this path
                // through the program, then we can't conclude anything about
                // prefix literals for this program. For example, if the regex
                // is `a|b*`, then the second alternate has no prefix to search
                // for. (`b*` matches the empty string!)
                return AlternateLiterals::empty();
            }
            if self.alts.num_bytes() + alts.num_bytes() > self.limit {
                // We've blown our budget. Give up.
                // We could do something a little smarter here and try to trim
                // the literals we've got here. (e.g., If every literal is two
                // characters, then it would be legal to remove the second char
                // from every literal.)
                return AlternateLiterals::empty();
            }
            self.alts.add_alternates(alts);
        }
        self.alts
    }
}

pub struct BuildRequiredLiterals<'a> {
    insts: &'a Program,
    limit: usize,
    alts: AlternateLiterals,
}

impl<'a> BuildRequiredLiterals<'a> {
    pub fn new(insts: &'a Program) -> Self {
        BuildRequiredLiterals {
            insts: insts,
            limit: 250,
            alts: AlternateLiterals { at_match: true, literals: vec![vec![]] },
        }
    }

    pub fn set_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }

    fn literals(mut self, mut pc: usize) -> AlternateLiterals {
        use prog::Inst::*;
        loop {
            let inst = &self.insts[pc];
            match *inst {
                Save(ref inst) => pc = inst.goto,
                Char(ref inst) => {
                    if !self.add_literal_char(inst.c) {
                        self.alts.at_match = false;
                        break;
                    }
                    pc = inst.goto;
                }
                Ranges(ref inst) => {
                    if !self.add_literal_char_ranges(inst) {
                        self.alts.at_match = false;
                        break;
                    }
                    pc = inst.goto;
                }
                Bytes(ref inst) => {
                    if !self.add_literal_byte_range(inst) {
                        self.alts.at_match = false;
                        break;
                    }
                    pc = inst.goto;
                }
                Split(_) | EmptyLook(_) | Match(_) => {
                    self.alts.at_match = self.insts.leads_to_match(pc);
                    break;
                }
            }
        }
        if self.alts.literals.len() == 1 && self.alts.literals[0].is_empty() {
            AlternateLiterals::empty()
        } else {
            self.alts
        }
    }

    fn add_literal_char(&mut self, c: char) -> bool {
        if self.alts.num_bytes() + 1 > self.limit {
            return false;
        }
        self.alts.add_literal_char(c);
        true
    }

    fn add_literal_char_ranges(&mut self, inst: &InstRanges) -> bool {
        // Compute roughly how many bytes will be in our literals following
        // the addition of the given ranges. If we blow our limit, then we
        // can't add *any* of them.
        let nchars = inst.num_chars();
        let new_byte_count = (self.alts.num_bytes() * nchars)
                             + (self.alts.literals.len() * nchars);
        if new_byte_count > self.limit {
            return false;
        }
        self.alts.add_literal_char_ranges(inst);
        true
    }

    fn add_literal_byte_range(&mut self, inst: &InstBytes) -> bool {
        // Compute roughly how many bytes will be in our literals following
        // the addition of the given range. If we blow our limit, then we
        // can't add anything.
        let nbytes = (inst.end as usize) - (inst.start as usize) + 1;
        let new_byte_count = (self.alts.num_bytes() * nbytes)
                             + (self.alts.literals.len() * nbytes);
        if new_byte_count > self.limit {
            return false;
        }
        self.alts.add_literal_byte_range(inst);
        true
    }
}

/// A prefix extracted from a compiled regular expression.
///
/// A regex prefix is a set of literal strings that *must* be matched at the
/// beginning of a regex in order for the entire regex to match.
///
/// There are a variety of ways to efficiently scan the search text for a
/// prefix. Currently, there are three implemented:
///
/// 1. The prefix is a single byte. Just use memchr.
/// 2. If the prefix is a set of two or more single byte prefixes, then
///    a single sparse map is created. Checking if there is a match is a lookup
///    in this map for each byte in the search text.
/// 3. In all other cases, build an Aho-Corasick automaton.
///
/// It's possible that there's room here for other substring algorithms,
/// such as Boyer-Moore for single-set prefixes greater than 1, or Rabin-Karp
/// for small sets of same-length prefixes.
#[derive(Clone)]
pub struct Literals {
    at_match: bool,
    matcher: LiteralMatcher,
}

#[derive(Clone)]
enum LiteralMatcher {
    /// No prefixes. (Never advances through the input.)
    Empty,
    /// A single byte prefix.
    Byte(u8),
    /// A set of two or more single byte prefixes.
    Bytes {
        chars: Vec<u8>,
        sparse: Vec<bool>,
    },
    /// A single substring. (Likely using Boyer-Moore with memchr.)
    Single(SingleSearch),
    /// An Aho-Corasick automaton.
    AC(FullAcAutomaton<Vec<u8>>),
}

impl Literals {
    /// Returns a matcher that never matches and never advances the input.
    pub fn empty() -> Self {
        Literals { at_match: false, matcher: LiteralMatcher::Empty }
    }

    /// Returns a matcher for literal prefixes in the given program.
    pub fn prefixes(prog: &Program) -> Self {
        BuildPrefixes::new(prog).literals().into_matcher()
    }

    /// Returns true if and only if a literal match corresponds to a match
    /// in the regex from which the literal was extracted.
    pub fn at_match(&self) -> bool {
        self.at_match && self.len() > 0
    }

    /// Find the position of a prefix in `haystack` if it exists.
    ///
    /// In the matching engines, we only actually need the starting index
    /// because the prefix is used to only skip ahead---the matching engine
    /// still needs to run over the prefix input. However, we return the ending
    /// location as well in case the prefix corresponds to the entire regex,
    /// in which case, you need the end of the match.
    pub fn find(&self, haystack: &[u8]) -> Option<(usize, usize)> {
        use self::LiteralMatcher::*;
        match self.matcher {
            Empty => Some((0, 0)),
            Byte(b) => memchr(b, haystack).map(|i| (i, i+1)),
            Bytes { ref sparse, ref chars } => {
                if chars.len() == 2 {
                    memchr2(chars[0], chars[1], haystack).map(|i| (i, i+1))
                } else if chars.len() == 3 {
                    let (b0, b1, b2) = (chars[0], chars[1], chars[2]);
                    memchr3(b0, b1, b2, haystack).map(|i| (i, i+1))
                } else {
                    find_singles(sparse, haystack)
                }
            }
            Single(ref searcher) => {
                searcher.find(haystack).map(|i| (i, i + searcher.pat.len()))
            }
            AC(ref aut) => {
                aut.find(haystack).next().map(|m| (m.start, m.end))
            }
        }
    }

    /// Returns true iff this prefix is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the number of prefixes in this machine.
    pub fn len(&self) -> usize {
        use self::LiteralMatcher::*;
        match self.matcher {
            Empty => 0,
            Byte(_) => 1,
            Bytes { ref chars, .. } => chars.len(),
            Single(_) => 1,
            AC(ref aut) => aut.len(),
        }
    }

    /// Return the approximate heap usage of literals in bytes.
    pub fn approximate_size(&self) -> usize {
        use self::LiteralMatcher::*;
        match self.matcher {
            Empty | Byte(_) => 0,
            Bytes { ref chars, ref sparse } => {
                (chars.len() * mem::size_of::<u8>())
                + (sparse.len() * mem::size_of::<bool>())
            }
            Single(ref single) => {
                (single.pat.len() * mem::size_of::<u8>())
                + (single.shift.len() * mem::size_of::<usize>())
            }
            AC(ref aut) => aut.heap_bytes(),
        }
    }

    /// Returns all of the literal participating in this machine.
    ///
    /// For debug/testing only! (It allocates.)
    #[allow(dead_code)]
    fn strings(&self) -> Vec<String> {
        self.byte_strings()
            .into_iter()
            .map(|p| String::from_utf8(p).unwrap())
            .collect()
    }

    #[allow(dead_code)]
    fn byte_strings(&self) -> Vec<Vec<u8>> {
        use self::LiteralMatcher::*;
        match self.matcher {
            Empty => vec![],
            Byte(b) => vec![vec![b]],
            Bytes { ref chars, .. } => {
                chars.iter().map(|&byte| vec![byte]).collect()
            }
            Single(ref searcher) => vec![searcher.pat.clone()],
            AC(ref aut) => aut.patterns().iter().cloned().collect(),
        }
    }
}

impl LiteralMatcher {
    fn new(mut alts: AlternateLiterals) -> Self {
        use self::LiteralMatcher::*;

        if alts.is_empty() {
            Empty
        } else if alts.distinct_single_bytes() >= 26 {
            // Why do we do this? Well, it's a heuristic to prevent thrashing.
            // Basically, if our literal matcher has lots of literals that are
            // a single byte, then we lose a lot of the benefits of fast
            // literal searching. In particular, single bytes have a high
            // probability of matching. In a regex that rarely matches, we end
            // up ping-ponging between the literal matcher and the regex engine
            // for every byte of input. That's bad juju.
            //
            // Note that we only count distinct starting bytes from literals of
            // length 1. For literals longer than that, we assume they have
            // a lower probability of matching.
            //
            // This particular heuristic would be triggered on, e.g.,
            // `[a-z].+`. The prefix here is a single byte that is very likely
            // to match on any given byte in the input, so it's quicker just
            // to let the matching engine process it.
            //
            // TODO(burntsushi): Consider lowering the threshold!
            Empty
        } else if alts.is_single_byte() {
            Byte(alts.literals[0][0])
        } else if alts.all_single_bytes() {
            let mut set = vec![false; 256];
            let mut bytes = vec![];
            for lit in alts.literals {
                bytes.push(lit[0]);
                set[lit[0] as usize] = true;
            }
            Bytes { chars: bytes, sparse: set }
        } else if alts.is_one_literal() {
            Single(SingleSearch::new(alts.literals.pop().unwrap()))
        } else {
            AC(AcAutomaton::new(alts.literals).into_full())
        }
    }
}

/// Provides an implementation of fast subtring search.
///
/// In particular, this uses Boyer-Moore-Horspool with Tim Raita's twist:
/// https://en.wikipedia.org/wiki/Raita_Algorithm
///
/// I'm skeptical of the utility here, because benchmarks suggest that it is
/// difficult to beat Aho-Corasick on random text. Namely, both algorithms are
/// dominated by the performance of `memchr` for the leading byte prefix.
/// With that said, BMH does seem to surpass AC when the search text gets
/// longer (see the `easy0_1MB` vs. `easy1_1MB` benchmarks).
///
/// More analysis needs to be done to test this on different search texts.
#[derive(Clone, Debug)]
pub struct SingleSearch {
    pat: Vec<u8>,
    shift: Vec<usize>,
}

impl SingleSearch {
    fn new(pat: Vec<u8>) -> SingleSearch {
        assert!(pat.len() >= 1);
        let mut shift = vec![pat.len(); 256];
        for i in 0..(pat.len() - 1) {
            shift[pat[i] as usize] = pat.len() - i - 1;
        }
        SingleSearch {
            pat: pat,
            shift: shift,
        }
    }

    fn find(&self, haystack: &[u8]) -> Option<usize> {
        let pat = &*self.pat;
        if haystack.len() < pat.len() {
            return None;
        }
        let mut i = match memchr(pat[0], haystack) {
            None => return None,
            Some(i) => i,
        };
        while i <= haystack.len() - pat.len() {
            let b = haystack[i + pat.len() - 1];
            if b == pat[pat.len() - 1]
               && haystack[i] == pat[0]
               && haystack[i + (pat.len() / 2)] == pat[pat.len() / 2]
               && pat == &haystack[i..i + pat.len()] {
                return Some(i);
            }
            i += self.shift[b as usize];
            i += match memchr(pat[0], &haystack[i..]) {
                None => return None,
                Some(i) => i,
            };
        }
        None
    }
}

/// A quick scan for multiple single byte prefixes using a sparse map.
fn find_singles(
    sparse: &[bool],
    text: &[u8],
) -> Option<(usize, usize)> {
    for (hi, &b) in text.iter().enumerate() {
        if sparse[b as usize] {
            return Some((hi, hi+1));
        }
    }
    None
}

impl fmt::Debug for AlternateLiterals {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut strings = vec![];
        for lit in &self.literals {
            strings.push(String::from_utf8_lossy(lit).into_owned());
        }
        f.debug_struct("AlternateLiterals")
         .field("at_match", &self.at_match)
         .field("literals", &strings)
         .finish()
    }
}

impl fmt::Debug for Literals {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LiteralMatcher::*;
        try!(write!(f, "complete? {}, matcher: ", self.at_match));
        match self.matcher {
            Empty => write!(f, "Empty"),
            Byte(b) => write!(f, "one byte: {:?}", b as char),
            Bytes { ref chars, .. } => {
                let chars: Vec<String> =
                    chars.iter()
                         .map(|&c| format!("{:?}", c as char))
                         .collect();
                write!(f, "alternate single bytes: {}", chars.join(", "))
            }
            Single(ref searcher) => write!(f, "{:?}", searcher),
            AC(ref aut) => write!(f, "{:?}", aut),
        }
    }
}

#[cfg(test)]
mod tests {
    use compile::Compiler;
    use super::{AlternateLiterals, BuildPrefixes};
    use syntax::Expr;

    macro_rules! prog {
        ($re:expr) => {{
            let expr = Expr::parse($re).unwrap();
            let prog = Compiler::new().compile(&[expr]).unwrap();
            prog
        }}
    }

    macro_rules! prefixes {
        ($re:expr) => {{
            let p = prog!($re);
            let prefixes = BuildPrefixes::new(&p).literals().into_matcher();
            assert!(!prefixes.at_match());
            prefixes.strings()
        }}
    }
    macro_rules! prefixes_complete {
        ($re:expr) => {{
            let p = prog!($re);
            let prefixes = BuildPrefixes::new(&p).literals().into_matcher();
            assert!(prefixes.at_match());
            prefixes.strings()
        }}
    }

    #[test]
    fn single() {
        assert_eq!(prefixes_complete!("a"), vec!["a"]);
        assert_eq!(prefixes_complete!("[a]"), vec!["a"]);
        assert_eq!(prefixes!("a+"), vec!["a"]);
        assert_eq!(prefixes!("(?:a)+"), vec!["a"]);
        assert_eq!(prefixes!("(a)+"), vec!["a"]);
    }

    #[test]
    fn single_alt() {
        assert_eq!(prefixes_complete!("a|b"), vec!["a", "b"]);
        assert_eq!(prefixes_complete!("b|a"), vec!["b", "a"]);
        assert_eq!(prefixes_complete!("[a]|[b]"), vec!["a", "b"]);
        assert_eq!(prefixes!("a+|b"), vec!["a", "b"]);
        assert_eq!(prefixes!("a|b+"), vec!["a", "b"]);
        assert_eq!(prefixes!("(?:a+)|b"), vec!["a", "b"]);
        assert_eq!(prefixes!("(a+)|b"), vec!["a", "b"]);
    }

    #[test]
    fn many() {
        assert_eq!(prefixes_complete!("abcdef"), vec!["abcdef"]);
        assert_eq!(prefixes!("abcdef+"), vec!["abcdef"]);
        assert_eq!(prefixes!("(?:abcdef)+"), vec!["abcdef"]);
        assert_eq!(prefixes!("(abcdef)+"), vec!["abcdef"]);
    }

    #[test]
    fn many_alt() {
        assert_eq!(prefixes_complete!("abc|def"), vec!["abc", "def"]);
        assert_eq!(prefixes_complete!("def|abc"), vec!["def", "abc"]);
        assert_eq!(prefixes!("abc+|def"), vec!["abc", "def"]);
        assert_eq!(prefixes!("abc|def+"), vec!["abc", "def"]);
        assert_eq!(prefixes!("(?:abc)+|def"), vec!["abc", "def"]);
        assert_eq!(prefixes!("(abc)+|def"), vec!["abc", "def"]);
    }

    #[test]
    fn class() {
        assert_eq!(prefixes_complete!("[0-9]"), vec![
            "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
        ]);
        assert_eq!(prefixes!("[0-9]+"), vec![
            "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
        ]);
    }

    #[test]
    fn preceding_alt() {
        assert_eq!(prefixes!("(?:a|b).+"), vec!["a", "b"]);
        assert_eq!(prefixes!("(a|b).+"), vec!["a", "b"]);
    }

    #[test]
    fn nested_alt() {
        assert_eq!(prefixes_complete!("(a|b|c|d)"),
                   vec!["a", "b", "c", "d"]);
        assert_eq!(prefixes_complete!("((a|b)|(c|d))"),
                   vec!["a", "b", "c", "d"]);
    }

    #[test]
    fn snowman() {
        assert_eq!(prefixes_complete!("☃"), vec!["☃"]);
    }

    macro_rules! alts {
        ($($s:expr),*) => {{
            AlternateLiterals {
                at_match: false,
                literals: vec![$($s.as_bytes().to_owned()),*],
            }
        }}
    }

    #[test]
    fn unambiguous() {
        let given = alts!("z", "azb");
        let expected = alts!("a", "z");
        assert_eq!(expected, given.unambiguous());

        let given = alts!("zaaaaaaaaaa", "aa");
        let expected = alts!("aa", "z");
        assert_eq!(expected, given.unambiguous());

        let given = alts!("Sherlock", "Watson");
        let expected = alts!("Sherlock", "Watson");
        assert_eq!(expected, given.unambiguous());

        let given = alts!("abc", "bc");
        let expected = alts!("a", "bc");
        assert_eq!(expected, given.unambiguous());

        let given = alts!("bc", "abc");
        let expected = alts!("a", "bc");
        assert_eq!(expected, given.unambiguous());

        let given = alts!("a", "aa");
        let expected = alts!("a");
        assert_eq!(expected, given.unambiguous());

        let given = alts!("ab", "a");
        let expected = alts!("a");
        assert_eq!(expected, given.unambiguous());
    }

    // That this test fails suggests that the literal finder needs to be
    // completely rewritten. Ug. It's not that it is wrong currently, but
    // it's not as good at finding literals as it should be.
    /*
    #[test]
    fn non_contiguous() {
        assert_eq!(prefixes_complete!("z(a|c)"), vec!["za", "zc"]);
    }
    */
}
