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
use dfa::DfaCache;
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
///
/// A compiled regular expression contains quite a bit more than justs its
/// opcodes. It also contains capture group names, literal prefixes, the
/// original regular expression string and some facts about the expression
/// (like whether it is anchored to the beginning or end of the search text).
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

/// A builder for compiling a regular expression program.
pub struct ProgramBuilder {
    re: String,
    compiler: Compiler,
}

impl ProgramBuilder {
    /// Create a new program builder for the given regular expression.
    ///
    /// Afer new is called, it is legal to call compile immediately. Default
    /// values for other knobs are set automatically.
    pub fn new(re: &str) -> Self {
        ProgramBuilder {
            re: re.to_owned(),
            compiler: Compiler::new(),
        }
    }

    /// Set a size limit that the compiler uses to limit the total number of
    /// bytes occupied by the opcodes for this regex.
    pub fn size_limit(mut self, size_limit: usize) -> Self {
        self.compiler = self.compiler.size_limit(size_limit);
        self
    }

    /// Enable compilation of a byte based program.
    ///
    /// By default, programs operate on Unicode codepoints.
    pub fn bytes(mut self, yes: bool) -> Self {
        self.compiler = self.compiler.bytes(yes);
        self
    }

    /// Enable compilation of a byte based DFA program.
    ///
    /// This does instruct the compiler to compile a byte based program, but
    /// it also does other things that are specifically required by the lazy
    /// DFA, such as adding a `.*?` before the first capture save for
    /// unanchored regular expressions.
    pub fn dfa(mut self, yes: bool) -> Self {
        self.compiler = self.compiler.dfa(yes);
        self
    }

    /// Compile the regular expression in reverse.
    ///
    /// This is generally only used by the lazy DFA to find the start location
    /// of a match.
    pub fn reverse(mut self, yes: bool) -> Self {
        self.compiler = self.compiler.reverse(yes);
        self
    }

    /// Compile the given regular expression under the given configuration.
    ///
    /// If the regular expression could not be compiled (e.g., it is too big),
    /// then return an error.
    pub fn compile(self) -> Result<Program, Error> {
        let expr = try!(syntax::Expr::parse(&self.re));
        let Compiled { insts, cap_names } = try!(self.compiler.compile(&expr));
        let (prefixes, anchored_begin, anchored_end) = (
            insts.prefix_matcher(),
            insts.anchored_begin(),
            insts.anchored_end(),
        );
        Ok(Program {
            original: self.re,
            insts: insts,
            cap_names: cap_names,
            prefixes: prefixes,
            anchored_begin: anchored_begin,
            anchored_end: anchored_end,
            cache: EngineCache::new(),
        })
    }
}

impl Program {
    /// Returns true if the set of literal prefixes implies a match and
    /// preserves leftmost first matching semantics.
    ///
    /// If this returns true, then it is possible to avoid running any of the
    /// NFA or DFA based matching engines entirely.
    pub fn is_prefix_match(&self) -> bool {
        self.prefixes.at_match() && self.prefixes.preserves_priority()
    }

    /// Returns true if the underlying program is reversed.
    pub fn is_reversed(&self) -> bool {
        self.insts.is_reversed()
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

    /// Return the approximate heap usage of this Program in bytes.
    ///
    /// Note that this does not include cached engine data.
    pub fn approximate_size(&self) -> usize {
        // ignore capture names
        self.original.len()
        + self.insts.approximate_size()
        + self.prefixes.approximate_size()
    }
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
