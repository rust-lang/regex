// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::fmt;

use aho_corasick::{Automaton, AcAutomaton, FullAcAutomaton};
use memchr::memchr;

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
pub enum Prefix {
    /// No prefixes. (Never advances through the input.)
    Empty,
    /// A single byte prefix.
    Single(u8),
    /// A set of two or more single byte prefixes.
    /// This could be reduced to a bitset, which would use only 8 bytes,
    /// but I don't think we care.
    Singles {
        chars: Vec<u8>,
        sparse: Vec<bool>,
    },
    /// A full Aho-Corasick DFA automaton.
    Automaton(FullAcAutomaton),
}

impl Prefix {
    /// Create a new prefix matching machine.
    pub fn new(pfxs: Vec<String>) -> Prefix {
        if pfxs.len() == 0 || pfxs[0].len() == 0 {
            Prefix::Empty
        } else if pfxs.len() == 1 && pfxs[0].len() == 1 {
            Prefix::Single(pfxs[0].as_bytes()[0])
        } else if pfxs.len() >= 2 && pfxs.iter().all(|s| s.len() == 1) {
            let mut set = vec![false; 256];
            let mut chars = vec![];
            for p in pfxs {
                chars.push(p.as_bytes()[0]);
                set[p.as_bytes()[0] as usize] = true;
            }
            Prefix::Singles { chars: chars, sparse: set }
        } else {
            Prefix::Automaton(AcAutomaton::new(pfxs).into_full())
        }
    }

    /// Find the position of a prefix in `haystack` if it exists.
    ///
    /// In the matching engines, we only actually need the starting index
    /// because the prefix is used to only skip ahead---the matching engine
    /// still needs to run over the prefix input. However, we return the ending
    /// location as well in case the prefix corresponds to the entire regex,
    /// in which case, you need the end of the match.
    pub fn find(&self, haystack: &str) -> Option<(usize, usize)> {
        use self::Prefix::*;
        match *self {
            Empty => Some((0, 0)),
            Single(b) => memchr(b, haystack.as_bytes()).map(|i| (i, i+1)),
            Singles { ref sparse, .. } => {
                find_singles(sparse, haystack.as_bytes())
            }
            Automaton(ref aut) => {
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
        match *self {
            Prefix::Empty => 0,
            Prefix::Single(_) => 1,
            Prefix::Singles { ref chars, .. } => chars.len(),
            Prefix::Automaton(ref aut) => aut.len(),
        }
    }

    /// Returns true iff the prefix match preserves priority.
    ///
    /// For example, given the alternation `ab|a` and the target string `ab`,
    /// does the prefix machine guarantee that `ab` will match? (A full
    /// Aho-Corasick automaton does not!)
    pub fn preserves_priority(&self) -> bool {
        match *self {
            Prefix::Empty => true,
            Prefix::Single(_) => true,
            Prefix::Singles{..} => true,
            Prefix::Automaton(ref aut) => {
                // Okay, so the automaton can respect priority in one
                // particular case: when every pattern is of the same length.
                // The trick is that the automaton will report the leftmost
                // match, which in this case, corresponds to the correct
                // match for the regex engine. If any other alternate matches
                // at the same position, then they must be exactly equivalent.

                // Guaranteed at least one prefix by construction, so use
                // that for the length.
                aut.patterns().iter().all(|p| p.len() == aut.pattern(0).len())
            }
        }
    }
}

/// A very quick scan for multiple single byte prefixes using a sparse map.
fn find_singles(sparse: &[bool], haystack: &[u8]) -> Option<(usize, usize)> {
    for (hi, &b) in haystack.iter().enumerate() {
        if sparse[b as usize] {
            return Some((hi, hi+1));
        }
    }
    None
}

impl fmt::Debug for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Prefix::Empty => write!(f, "Empty"),
            Prefix::Single(b) => write!(f, "{:?}", b as char),
            Prefix::Singles { ref chars, .. } => {
                let chars: Vec<String> =
                    chars.iter()
                         .map(|&c| format!("{:?}", c as char))
                         .collect();
                write!(f, "{}", chars.connect(", "))
            }
            Prefix::Automaton(ref aut) => write!(f, "{:?}", aut),
        }
    }
}
