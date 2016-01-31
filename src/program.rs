// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use syntax;

use backtrack::BacktrackCache;
use compile::{Compiled, Compiler};
use inst::Insts;
use nfa::NfaCache;
use pool::{Pool, PoolGuard};
use literals::Literals;
use Error;


/// Program represents a compiled regular expression. Once an expression is
/// compiled, its representation is immutable and will never change.
/// (Well, almost. In fact, the matching engines cache state that can be
/// reused on subsequent searches. But this is interior mutability that
/// shouldn't be observable by the caller.)
#[derive(Clone, Debug)]
pub struct Program {
    /// The original regular expression string.
    pub original: String,
    /// A sequence of instructions.
    pub insts: Insts,
    /// The sequence of capture group names. There is an entry for each capture
    /// group index and a name exists only if the capture group is named.
    pub cap_names: Vec<Option<String>>,
    /// If the regular expression requires a literal prefix in order to have a
    /// match, that prefix is stored here as a DFA.
    pub prefixes: Literals,
    /// True iff program is anchored at the beginning.
    pub anchored_begin: bool,
    /// True iff program is anchored at the end.
    pub anchored_end: bool,
    /// Cached reusable state for matching engines.
    pub cache: EngineCache,
}

impl Program {
    pub fn bytes(re: &str, size_limit: usize) -> Result<Program, Error> {
        let expr = try!(syntax::Expr::parse(re));
        let compiler = Compiler::new(size_limit, true);
        Ok(Program::new(re, try!(compiler.compile(&expr))))
    }

    pub fn unicode(re: &str, size_limit: usize) -> Result<Program, Error> {
        let expr = try!(syntax::Expr::parse(re));
        let compiler = Compiler::new(size_limit, false);
        Ok(Program::new(re, try!(compiler.compile(&expr))))
    }

    fn new(re: &str, compiled: Compiled) -> Program {
        let Compiled { insts, cap_names } = compiled;
        let (prefixes, anchored_begin, anchored_end) = (
            insts.prefix_matcher(),
            insts.anchored_begin(),
            insts.anchored_end(),
        );
        Program {
            original: re.to_owned(),
            insts: insts,
            cap_names: cap_names,
            prefixes: prefixes,
            anchored_begin: anchored_begin,
            anchored_end: anchored_end,
            cache: EngineCache::new(),
        }
    }

    pub fn is_prefix_match(&self) -> bool {
        self.prefixes.at_match() && self.prefixes.preserves_priority()
    }

    /// Returns the total number of capture groups in the regular expression.
    /// This includes the zeroth capture.
    pub fn num_captures(&self) -> usize {
        self.cap_names.len()
    }

    /// Allocate new capture groups.
    pub fn alloc_captures(&self) -> Vec<Option<usize>> {
        vec![None; 2 * self.num_captures()]
    }

    /// Retrieve cached state for NFA execution.
    pub fn cache_nfa(&self) -> PoolGuard<NfaCache> {
        self.cache.nfa.get()
    }

    /// Retrieve cached state for backtracking execution.
    pub fn cache_backtrack(&self) -> PoolGuard<BacktrackCache> {
        self.cache.backtrack.get()
    }
}

/// EngineCache maintains reusable allocations for each matching engine
/// available to a particular program.
///
/// The allocations are created lazily, so we don't pay for caches that
/// aren't used.
#[derive(Debug)]
pub struct EngineCache {
    nfa: Pool<NfaCache>,
    backtrack: Pool<BacktrackCache>,
}

impl EngineCache {
    fn new() -> Self {
        EngineCache {
            nfa: Pool::new(Box::new(move || NfaCache::new())),
            backtrack: Pool::new(Box::new(move || BacktrackCache::new())),
        }
    }
}

impl Clone for EngineCache {
    fn clone(&self) -> EngineCache {
        EngineCache::new()
    }
}
