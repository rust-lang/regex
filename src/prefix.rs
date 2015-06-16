// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use aho_corasick::AcAutomaton;
use memchr::memchr;

/// A prefix extracted from a compiled regular expression.
///
/// A regex prefix is a set of literal strings that *must* be matched at the
/// beginning of a regex in order for the entire regex to match.
///
/// There are a variety of ways to efficiently scan the search text for a
/// prefix. Currently, there are two implemented:
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
#[derive(Clone, Debug)]
pub enum Prefix {
    /// No prefixes. (Never advances through the input.)
    Empty,
    /// A single byte prefix.
    Single(u8),
    /// A set of two or more single byte prefixes.
    /// This could be reduced to a bitset, which would use only 8 bytes,
    /// but I don't think we care.
    Singles(Vec<bool>),
    /// A full Aho-Corasick DFA automaton.
    Automaton(AcAutomaton),
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
            for p in pfxs {
                set[p.as_bytes()[0] as usize] = true;
            }
            Prefix::Singles(set)
        } else {
            Prefix::Automaton(AcAutomaton::new(pfxs))
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
            Singles(ref pats) => find_singles(pats, haystack.as_bytes()),
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
            Prefix::Singles(ref pats) => pats.len(),
            Prefix::Automaton(ref aut) => aut.len(),
        }
    }
}

/// A very quick scan for multiple single byte prefixes using a sparse map.
fn find_singles(pats: &[bool], haystack: &[u8]) -> Option<(usize, usize)> {
    for (hi, &b) in haystack.iter().enumerate() {
        if pats[b as usize] {
            return Some((hi, hi+1));
        }
    }
    None
}
