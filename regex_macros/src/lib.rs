// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! This crate provides the `regex!` macro. Its use is documented in the
//! `regex` crate.

#![doc(html_logo_url = "http://www.rust-lang.org/logos/rust-logo-128x128-blk-v2.png",
       html_favicon_url = "http://www.rust-lang.org/favicon.ico",
       html_root_url = "http://doc.rust-lang.org/nightly/")]

#![feature(plugin_registrar, quote, rustc_private)]

extern crate regex;
extern crate regex_syntax;
extern crate rustc_plugin;
extern crate syntax;

use std::collections::BTreeMap;
use std::usize;

use syntax::ast;
use syntax::codemap;
use syntax::ext::build::AstBuilder;
use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult};
use syntax::parse::token;
use syntax::print::pprust;
use syntax::fold::Folder;
use syntax::ptr::P;

use rustc_plugin::Registry;

use regex::internal::{Compiler, EmptyLook, Inst, Program};
use regex_syntax::Expr;

/// For the `regex!` syntax extension. Do not use.
#[plugin_registrar]
#[doc(hidden)]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("regex", native);
}

/// Generates specialized code for the Pike VM for a particular regular
/// expression.
///
/// There are two primary differences between the code generated here and the
/// general code in vm.rs.
///
/// 1. All heap allocation is removed. Sized vector types are used instead.
///    Care must be taken to make sure that these vectors are not copied
///    gratuitously. (If you're not sure, run the benchmarks. They will yell
///    at you if you do.)
/// 2. The main `match instruction { ... }` expressions are replaced with more
///    direct `match pc { ... }`. The generators can be found in
///    `step_insts` and `add_insts`.
///
/// It is strongly recommended to read the dynamic implementation in vm.rs
/// first before trying to understand the code generator. The implementation
/// strategy is identical and vm.rs has comments and will be easier to follow.
fn native(cx: &mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree])
          -> Box<MacResult+'static> {
    let regex = match parse(cx, tts) {
        Some(r) => r,
        // error is logged in 'parse' with cx.span_err
        None => return DummyResult::any(sp),
    };
    // We use the largest possible size limit because this is happening at
    // compile time. We trust the programmer.
    let expr = match Expr::parse(&regex) {
        Ok(expr) => expr,
        Err(err) => {
            cx.span_err(sp, &err.to_string());
            return DummyResult::any(sp)
        }
    };
    let prog = match Compiler::new().size_limit(usize::MAX).compile(&[expr]) {
        Ok(re) => re,
        Err(err) => {
            cx.span_err(sp, &err.to_string());
            return DummyResult::any(sp)
        }
    };
    let names = prog.captures.iter().cloned().collect();
    let mut gen = NfaGen {
        cx: &*cx,
        sp: sp,
        prog: prog,
        names: names,
        original: regex,
    };
    MacEager::expr(gen.code())
}

struct NfaGen<'a> {
    cx: &'a ExtCtxt<'a>,
    sp: codemap::Span,
    prog: Program,
    names: Vec<Option<String>>,
    original: String,
}

impl<'a> NfaGen<'a> {
    fn code(&mut self) -> P<ast::Expr> {
        // Most or all of the following things are used in the quasiquoted
        // expression returned.
        let num_cap_locs = 2 * self.prog.captures.len();
        let num_insts = self.prog.len();
        let cap_names = self.vec_expr(self.names.iter(),
            &mut |cx, name| match *name {
                Some(ref name) => {
                    let name = &**name;
                    quote_expr!(cx, Some($name))
                }
                None => cx.expr_none(self.sp),
            }
        );
        let capture_name_idx = {
            let mut capture_name_idx = BTreeMap::new();
            for (i, name) in self.names.iter().enumerate() {
                if let Some(ref name) = *name {
                    capture_name_idx.insert(name.to_owned(), i);
                }
            }
            self.vec_expr(capture_name_idx.iter(),
                &mut |cx, (name, group_idx)|
                    quote_expr!(cx, ($name, $group_idx))
            )
        };

        let is_anchored_start = self.prog.is_anchored_start;
        let step_insts = self.step_insts();
        let add_insts = self.add_insts();
        let regex = &*self.original;

        quote_expr!(self.cx, {
// When `regex!` is bound to a name that is not used, we have to make sure
// that dead_code warnings don't bubble up to the user from the generated
// code. Therefore, we suppress them by allowing dead_code. The effect is that
// the user is only warned about *their* unused variable/code, and not the
// unused code generated by regex!. See #14185 for an example.
#[allow(dead_code)]
static CAPTURES: &'static [Option<&'static str>] = &$cap_names;
#[allow(dead_code)]
static CAPTURE_NAME_IDX: &'static [(&'static str, usize)] = &$capture_name_idx;

#[allow(dead_code)]
fn exec<'t>(
    mut caps: &mut [Option<usize>],
    input: &'t str,
    start: usize,
) -> bool {
    #![allow(unused_imports)]
    #![allow(unused_mut)]

    use regex::internal::{Char, CharInput, InputAt, Input, Inst};

    let input = CharInput::new(input.as_bytes());
    let at = input.at(start);
    return Nfa {
        input: input,
        ncaps: caps.len(),
    }.exec(&mut NfaThreads::new(), &mut caps, at);

    struct Nfa<'t> {
        input: CharInput<'t>,
        ncaps: usize,
    }

    impl<'t> Nfa<'t> {
        #[allow(unused_variables)]
        fn exec(
            &mut self,
            mut q: &mut NfaThreads,
            mut caps: &mut [Option<usize>],
            mut at: InputAt,
        ) -> bool {
            let mut matched = false;
            let (mut clist, mut nlist) = (&mut q.clist, &mut q.nlist);
            clist.empty(); nlist.empty();
'LOOP:      loop {
                if clist.size == 0 {
                    if matched || (!at.is_start() && $is_anchored_start) {
                        break;
                    }
                    // TODO: Prefix matching... Hmm.
                    // Prefix matching now uses a DFA, so I think this is
                    // going to require encoding that DFA statically.
                }
                if clist.size == 0 || (!$is_anchored_start && !matched) {
                    self.add(clist, &mut caps, 0, at);
                }
                let at_next = self.input.at(at.next_pos());
                for i in 0..clist.size {
                    let pc = clist.pc(i);
                    let tcaps = clist.caps(i);
                    if self.step(nlist, caps, tcaps, pc, at, at_next) {
                        matched = true;
                        if caps.len() == 0 {
                            break 'LOOP;
                        }
                        break;
                    }
                }
                if at.char().is_none() {
                    break;
                }
                at = at_next;
                ::std::mem::swap(&mut clist, &mut nlist);
                nlist.empty();
            }
            matched
        }

        // Sometimes `nlist` is never used (for empty regexes).
        #[allow(unused_variables)]
        #[inline]
        fn step(
            &self,
            nlist: &mut Threads,
            caps: &mut [Option<usize>],
            thread_caps: &mut [Option<usize>],
            pc: usize,
            at: InputAt,
            at_next: InputAt,
        ) -> bool {
            $step_insts;
            false
        }

        fn add(
            &self,
            nlist: &mut Threads,
            thread_caps: &mut [Option<usize>],
            pc: usize,
            at: InputAt,
        ) {
            if nlist.contains(pc) {
                return;
            }
            let ti = nlist.add(pc);
            $add_insts
        }
    }

    struct NfaThreads {
        clist: Threads,
        nlist: Threads,
    }

    struct Threads {
        dense: [Thread; $num_insts],
        sparse: [usize; $num_insts],
        size: usize,
    }

    struct Thread {
        pc: usize,
        caps: [Option<usize>; $num_cap_locs],
    }

    impl NfaThreads {
        fn new() -> NfaThreads {
            NfaThreads {
                clist: Threads::new(),
                nlist: Threads::new(),
            }
        }

        fn swap(&mut self) {
            ::std::mem::swap(&mut self.clist, &mut self.nlist);
        }
    }

    impl Threads {
        fn new() -> Threads {
            Threads {
                // These unsafe blocks are used for performance reasons, as it
                // gives us a zero-cost initialization of a sparse set. The
                // trick is described in more detail here:
                // http://research.swtch.com/sparse
                // The idea here is to avoid initializing threads that never
                // need to be initialized, particularly for larger regexs with
                // a lot of instructions.
                dense: unsafe { ::std::mem::uninitialized() },
                sparse: unsafe { ::std::mem::uninitialized() },
                size: 0,
            }
        }

        #[inline]
        fn add(&mut self, pc: usize) -> usize {
            let i = self.size;
            self.dense[i].pc = pc;
            self.sparse[pc] = i;
            self.size += 1;
            i
        }

        #[inline]
        fn thread(&mut self, i: usize) -> &mut Thread {
            &mut self.dense[i]
        }

        #[inline]
        fn contains(&self, pc: usize) -> bool {
            let s = unsafe { ::std::ptr::read_volatile(&self.sparse[pc]) };
            s < self.size && self.dense[s].pc == pc
        }

        #[inline]
        fn empty(&mut self) {
            self.size = 0;
        }

        #[inline]
        fn pc(&self, i: usize) -> usize {
            self.dense[i].pc
        }

        #[inline]
        fn caps<'r>(&'r mut self, i: usize) -> &'r mut [Option<usize>] {
            &mut self.dense[i].caps
        }
    }
}

::regex::Regex(::regex::internal::_Regex::Plugin(::regex::internal::Plugin {
    original: $regex,
    names: &CAPTURES,
    groups: &CAPTURE_NAME_IDX,
    prog: exec,
}))
        })
    }

    // Generates code for the `add` method, which is responsible for adding
    // zero-width states to the next queue of states to visit.
    fn add_insts(&self) -> P<ast::Expr> {
        let arms = self.prog.iter().enumerate().map(|(pc, inst)| {
            let body = match *inst {
                Inst::EmptyLook(ref inst) => {
                    let nextpc = inst.goto;
                    match inst.look {
                        EmptyLook::StartLine => {
                            quote_expr!(self.cx, {
                                let prev = self.input.previous_char(at);
                                if prev.is_none() || prev == '\n' {
                                    self.add(nlist, thread_caps, $nextpc, at);
                                }
                            })
                        }
                        EmptyLook::EndLine => {
                            quote_expr!(self.cx, {
                                if at.char().is_none() || at.char() == '\n' {
                                    self.add(nlist, thread_caps, $nextpc, at);
                                }
                            })
                        }
                        EmptyLook::StartText => {
                            quote_expr!(self.cx, {
                                let prev = self.input.previous_char(at);
                                if prev.is_none() {
                                    self.add(nlist, thread_caps, $nextpc, at);
                                }
                            })
                        }
                        EmptyLook::EndText => {
                            quote_expr!(self.cx, {
                                if at.char().is_none() {
                                    self.add(nlist, thread_caps, $nextpc, at);
                                }
                            })
                        }
                        EmptyLook::WordBoundary
                        | EmptyLook::NotWordBoundary => {
                            let m = if inst.look == EmptyLook::WordBoundary {
                                quote_expr!(self.cx, { w1 ^ w2 })
                            } else {
                                quote_expr!(self.cx, { !(w1 ^ w2) })
                            };
                            quote_expr!(self.cx, {
                                let prev = self.input.previous_char(at);
                                let w1 = prev.is_word_char();
                                let w2 = at.char().is_word_char();
                                if $m {
                                    self.add(nlist, thread_caps, $nextpc, at);
                                }
                            })
                        }
                        EmptyLook::WordBoundaryAscii
                        | EmptyLook::NotWordBoundaryAscii => {
                            unreachable!()
                        }
                    }
                }
                Inst::Save(ref inst) => {
                    let nextpc = inst.goto;
                    let slot = inst.slot;
                    quote_expr!(self.cx, {
                        if $slot >= self.ncaps {
                            self.add(nlist, thread_caps, $nextpc, at);
                        } else {
                            let old = thread_caps[$slot];
                            thread_caps[$slot] = Some(at.pos());
                            self.add(nlist, thread_caps, $nextpc, at);
                            thread_caps[$slot] = old;
                        }
                    })
                }
                Inst::Split(ref inst) => {
                    let (x, y) = (inst.goto1, inst.goto2);
                    quote_expr!(self.cx, {
                        self.add(nlist, thread_caps, $x, at);
                        self.add(nlist, thread_caps, $y, at);
                    })
                }
                // For Match, Char, Ranges
                _ => quote_expr!(self.cx, {
                    let mut t = &mut nlist.thread(ti);
                    for (slot, val) in t.caps.iter_mut().zip(thread_caps.iter()) {
                        *slot = *val;
                    }
                }),
            };
            self.arm_inst(pc, body)
        }).collect::<Vec<ast::Arm>>();
        self.match_insts(arms)
    }

    // Generates the code for the `step` method, which processes all states
    // in the current queue that consume a single character.
    fn step_insts(&self) -> P<ast::Expr> {
        let arms = self.prog.iter().enumerate().map(|(pc, inst)| {
            let body = match *inst {
                Inst::Match(_) => quote_expr!(self.cx, {
                    for (slot, val) in caps.iter_mut().zip(thread_caps.iter()) {
                        *slot = *val;
                    }
                    return true;
                }),
                Inst::Char(ref inst) => {
                    let nextpc = inst.goto;
                    let c = inst.c;
                    quote_expr!(self.cx, {
                        if $c == at.char() {
                            self.add(nlist, thread_caps, $nextpc, at_next);
                        }
                        return false;
                    })
                }
                Inst::Ranges(ref inst) => {
                    let match_class = self.match_class(&inst.ranges);
                    let nextpc = inst.goto;
                    quote_expr!(self.cx, {
                        let mut c = at.char();
                        if let Some(c) = c.as_char() {
                            if $match_class {
                                self.add(nlist, thread_caps, $nextpc, at_next);
                            }
                        }
                        return false;
                    })
                }
                // EmptyLook, Save, Jump, Split
                _ => quote_expr!(self.cx, { return false; }),
            };
            self.arm_inst(pc, body)
        }).collect::<Vec<ast::Arm>>();

        self.match_insts(arms)
    }

    // Translates a character class into a match expression.
    // This avoids a binary search (and is hopefully replaced by a jump
    // table).
    fn match_class(&self, ranges: &[(char, char)]) -> P<ast::Expr> {
        let mut arms = ranges.iter().map(|&(start, end)| {
            let pat = self.cx.pat(
                self.sp, ast::PatKind::Range(
                    quote_expr!(self.cx, $start), quote_expr!(self.cx, $end)));
            self.cx.arm(self.sp, vec!(pat), quote_expr!(self.cx, true))
        }).collect::<Vec<ast::Arm>>();

        arms.push(self.wild_arm_expr(quote_expr!(self.cx, false)));
        let match_on = quote_expr!(self.cx, c);
        self.cx.expr_match(self.sp, match_on, arms)
    }

    // Generates code for checking a literal prefix of the search string.
    // The code is only generated if the regex *has* a literal prefix.
    // Otherwise, a no-op is returned.
    // fn check_prefix(&self) -> P<ast::Expr> {
        // if self.prog.prefixes.len() == 0 {
            // self.empty_block()
        // } else {
            // quote_expr!(self.cx,
                // if clist.size == 0 {
                    // let haystack = &self.input.as_bytes()[self.ic..];
                    // match find_prefix(prefix_bytes, haystack) {
                        // None => break,
                        // Some(i) => {
                            // self.ic += i;
                            // next_ic = self.chars.set(self.ic);
                        // }
                    // }
                // }
            // )
        // }
    // }

    // Builds a `match pc { ... }` expression from a list of arms, specifically
    // for matching the current program counter with an instruction.
    // A wild-card arm is automatically added that executes a no-op. It will
    // never be used, but is added to satisfy the compiler complaining about
    // non-exhaustive patterns.
    fn match_insts(&self, mut arms: Vec<ast::Arm>) -> P<ast::Expr> {
        arms.push(self.wild_arm_expr(self.empty_block()));
        self.cx.expr_match(self.sp, quote_expr!(self.cx, pc), arms)
    }

    fn empty_block(&self) -> P<ast::Expr> {
        quote_expr!(self.cx, {})
    }

    // Creates a match arm for the instruction at `pc` with the expression
    // `body`.
    fn arm_inst(&self, pc: usize, body: P<ast::Expr>) -> ast::Arm {
        let pc_pat = self.cx.pat_lit(self.sp, quote_expr!(self.cx, $pc));

        self.cx.arm(self.sp, vec!(pc_pat), body)
    }

    // Creates a wild-card match arm with the expression `body`.
    fn wild_arm_expr(&self, body: P<ast::Expr>) -> ast::Arm {
        ast::Arm {
            attrs: vec!(),
            pats: vec!(P(ast::Pat{
                id: ast::DUMMY_NODE_ID,
                span: self.sp,
                node: ast::PatKind::Wild,
            })),
            guard: None,
            body: body,
        }
    }

    // Converts `xs` to a `[x1, x2, .., xN]` expression by calling `to_expr`
    // on each element in `xs`.
    fn vec_expr<T, It: Iterator<Item=T>>(
        &self,
        xs: It,
        to_expr: &mut FnMut(&ExtCtxt, T) -> P<ast::Expr>,
    ) -> P<ast::Expr> {
        let exprs = xs.map(|x| to_expr(self.cx, x)).collect();
        self.cx.expr_vec(self.sp, exprs)
    }
}

/// Looks for a single string literal and returns it.
/// Otherwise, logs an error with cx.span_err and returns None.
fn parse(cx: &mut ExtCtxt, tts: &[ast::TokenTree]) -> Option<String> {
    let mut parser = cx.new_parser_from_tts(tts);
    if let Ok(expr) = parser.parse_expr() {
        let entry = cx.expander().fold_expr(expr);
        let regex = match entry.node {
            ast::ExprKind::Lit(ref lit) => {
                match lit.node {
                    ast::LitKind::Str(ref s, _) => s.to_string(),
                    _ => {
                        cx.span_err(entry.span, &format!(
                            "expected string literal but got `{}`",
                            pprust::lit_to_string(&**lit)));
                        return None
                    }
                }
            }
            _ => {
                cx.span_err(entry.span, &format!(
                    "expected string literal but got `{}`",
                    pprust::expr_to_string(&*entry)));
                return None
            }
        };
        if !parser.eat(&token::Eof) {
            cx.span_err(parser.span, "only one string literal allowed");
            return None;
        }
        Some(regex)
    } else {
        cx.parse_sess().span_diagnostic.err("failure parsing token tree");
        None
    }
}
