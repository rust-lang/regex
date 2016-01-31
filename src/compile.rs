// Copyright 2014-2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::iter;

use syntax::{Expr, Repeater, CharClass, ClassRange};
use utf8_ranges::{Utf8Sequence, Utf8Sequences};

use Error;
use inst::{
    Insts, Inst, InstIdx, EmptyLook,
    InstSave, InstSplit, InstEmptyLook, InstChar, InstRanges, InstBytes,
};

pub struct Compiled {
    pub insts: Insts,
    pub cap_names: Vec<Option<String>>,
}

type InstHoleIdx = InstIdx;

type CompileResult = Result<Hole, Error>;

pub struct Compiler {
    size_limit: usize,
    insts: Vec<MaybeInst>,
    cap_names: Vec<Option<String>>,
    seen_caps: HashSet<usize>,
    bytes: bool,
}

impl Compiler {
    /// Create a new regular expression compiler.
    ///
    /// The size of the resulting progrom is limited by size_limit. If the
    /// program exceeds the given size (in bytes), then compilation will return
    /// an error.
    ///
    /// If bytes is true, then the program is compiled as a byte based
    /// automaton, which incorporates UTF-8 decoding into the machine. If it's
    /// false, then the automaton is Unicode scalar value based, e.g., an
    /// engine utilizing such an automaton is resposible for UTF-8 decoding.
    pub fn new(size_limit: usize, bytes: bool) -> Compiler {
        Compiler {
            size_limit: size_limit,
            insts: vec![],
            cap_names: vec![None],
            seen_caps: HashSet::new(),
            bytes: bytes,
        }
    }

    pub fn compile(mut self, expr: &Expr) -> Result<Compiled, Error> {
        let hole = try!(self.c_capture(0, expr));
        self.fill_to_next(hole);
        self.push_compiled(Inst::Match);

        let insts = self.insts.into_iter().map(|inst| inst.unwrap()).collect();
        Ok(Compiled {
            insts: Insts::new(insts, self.bytes),
            cap_names: self.cap_names,
        })
    }

    fn c(&mut self, expr: &Expr) -> CompileResult {
        use inst;
        use syntax::Expr::*;

        try!(self.check_size());
        match *expr {
            Empty => Ok(Hole::None),
            Literal { ref chars, casei } => self.c_literal(chars, casei),
            AnyChar => self.c_class(&[ClassRange {
                start: '\x00',
                end: '\u{10ffff}',
            }]),
            AnyCharNoNL => {
                self.c_class(&[
                    ClassRange { start: '\x00', end: '\x09' },
                    ClassRange { start: '\x0b', end: '\u{10ffff}' },
                ])
            }
            Class(ref cls) => {
                self.c_class(cls)
            }
            StartLine => self.c_empty_look(inst::EmptyLook::StartLine),
            EndLine => self.c_empty_look(inst::EmptyLook::EndLine),
            StartText => self.c_empty_look(inst::EmptyLook::StartText),
            EndText => self.c_empty_look(inst::EmptyLook::EndText),
            WordBoundary => self.c_empty_look(inst::EmptyLook::WordBoundary),
            NotWordBoundary => {
                self.c_empty_look(inst::EmptyLook::NotWordBoundary)
            }
            Group { ref e, i: None, name: None } => self.c(e),
            Group { ref e, i, ref name } => {
                // it's impossible to have a named capture without an index
                let i = i.expect("capture index");
                if !self.seen_caps.contains(&i) {
                    self.cap_names.push(name.clone());
                    self.seen_caps.insert(i);
                }
                self.c_capture(2 * i, e)
            }
            Concat(ref es) => self.c_concat(es.iter()),
            Alternate(ref es) => self.c_alternate(&**es),
            Repeat { ref e, r, greedy } => self.c_repeat(e, r, greedy),
        }
    }

    fn c_capture(&mut self, first_slot: usize, expr: &Expr) -> CompileResult {
        let hole = self.push_hole(InstHole::Save { slot: first_slot });
        self.fill_to_next(hole);

        let hole = try!(self.c(expr));
        self.fill_to_next(hole);

        Ok(self.push_hole(InstHole::Save { slot: first_slot + 1 }))
    }

    fn c_literal(&mut self, chars: &[char], casei: bool) -> CompileResult {
        assert!(!chars.is_empty());
        if casei {
            let mut prev_hole = Hole::None;
            for &c in chars {
                self.fill_to_next(prev_hole);
                let class = CharClass::new(vec![
                    ClassRange { start: c, end: c },
                ]).case_fold();
                prev_hole = try!(self.c_class(&class));
            }
            Ok(prev_hole)
        } else {
            let mut prev_hole = Hole::None;
            for &c in chars {
                self.fill_to_next(prev_hole);
                prev_hole = try!(self.c_class(&[ClassRange {
                    start: c,
                    end: c,
                }]));
            }
            Ok(prev_hole)
        }
    }

    fn c_class(&mut self, ranges: &[ClassRange]) -> CompileResult {
        if self.bytes {
            CompileClass {
                c: self,
                ranges: ranges,
                suffix_cache: HashMap::new(),
            }.compile()
        } else {
            let ranges: Vec<(char, char)> =
                ranges.iter().map(|r| (r.start, r.end)).collect();
            Ok(if ranges.len() == 1 && ranges[0].0 == ranges[0].1 {
                self.push_hole(InstHole::Char { c: ranges[0].0 })
            } else {
                self.push_hole(InstHole::Ranges { ranges: ranges })
            })
        }
    }

    fn c_empty_look(&mut self, look: EmptyLook) -> CompileResult {
        Ok(self.push_hole(InstHole::EmptyLook { look: look }))
    }

    fn c_concat<'a, I>(&mut self, exprs: I) -> CompileResult
            where I: IntoIterator<Item=&'a Expr> {
        let mut prev_hole = Hole::None;
        for e in exprs {
            self.fill_to_next(prev_hole);
            prev_hole = try!(self.c(e));
        }
        Ok(prev_hole)
    }

    fn c_alternate(&mut self, exprs: &[Expr]) -> CompileResult {
        assert!(exprs.len() >= 2, "alternates must have at least 2 exprs");
        let mut holes = vec![];
        for e in &exprs[0..exprs.len() - 1] {
            let split = self.push_split_hole();
            let goto1 = self.insts.len();
            holes.push(try!(self.c(e)));
            let goto2 = self.insts.len();
            self.fill_split(split, Some(goto1), Some(goto2));
        }
        holes.push(try!(self.c(&exprs[exprs.len() - 1])));
        Ok(Hole::Many(holes))
    }

    fn c_repeat(
        &mut self,
        expr: &Expr,
        kind: Repeater,
        greedy: bool,
    ) -> CompileResult {
        match kind {
            Repeater::ZeroOrOne => self.c_repeat_zero_or_one(expr, greedy),
            Repeater::ZeroOrMore => self.c_repeat_zero_or_more(expr, greedy),
            Repeater::OneOrMore => self.c_repeat_one_or_more(expr, greedy),
            Repeater::Range { min, max: None } => {
                self.c_repeat_range_min_or_more(expr, greedy, min)
            }
            Repeater::Range { min, max: Some(max) } => {
                self.c_repeat_range(expr, greedy, min, max)
            }
        }
    }

    fn c_repeat_zero_or_one(
        &mut self,
        expr: &Expr,
        greedy: bool,
    ) -> CompileResult {
        let split = self.push_split_hole();
        let goto1 = self.insts.len();
        let hole1 = try!(self.c(expr));

        let hole2 = if greedy {
            self.fill_split(split, Some(goto1), None)
        } else {
            self.fill_split(split, None, Some(goto1))
        };
        Ok(Hole::Many(vec![hole1, hole2]))
    }

    fn c_repeat_zero_or_more(
        &mut self,
        expr: &Expr,
        greedy: bool,
    ) -> CompileResult {
        let goto_split = self.insts.len();
        let split = self.push_split_hole();
        let goto_rep_expr = self.insts.len();
        let hole_rep_expr = try!(self.c(expr));

        self.fill(hole_rep_expr, goto_split);
        Ok(if greedy {
            self.fill_split(split, Some(goto_rep_expr), None)
        } else {
            self.fill_split(split, None, Some(goto_rep_expr))
        })
    }

    fn c_repeat_one_or_more(
        &mut self,
        expr: &Expr,
        greedy: bool,
    ) -> CompileResult {
        let goto_rep_expr = self.insts.len();
        let hole_rep_expr = try!(self.c(expr));
        self.fill_to_next(hole_rep_expr);
        let split = self.push_split_hole();

        Ok(if greedy {
            self.fill_split(split, Some(goto_rep_expr), None)
        } else {
            self.fill_split(split, None, Some(goto_rep_expr))
        })
    }

    fn c_repeat_range_min_or_more(
        &mut self,
        expr: &Expr,
        greedy: bool,
        min: u32,
    ) -> CompileResult {
        let min = u32_to_usize(min);
        let hole = try!(self.c_concat(iter::repeat(expr).take(min)));
        self.fill_to_next(hole);
        self.c_repeat_zero_or_more(expr, greedy)
    }

    fn c_repeat_range(
        &mut self,
        expr: &Expr,
        greedy: bool,
        min: u32,
        max: u32,
    ) -> CompileResult {
        let (min, max) = (u32_to_usize(min), u32_to_usize(max));
        let hole = try!(self.c_concat(iter::repeat(expr).take(min)));
        if min == max {
            return Ok(hole);
        }
        self.fill_to_next(hole);
        // It is much simpler to compile, e.g., `a{2,5}` as:
        //
        //     aaa?a?a?
        //
        // But you end up with a sequence of instructions like this:
        //
        //     0: 'a'
        //     1: 'a',
        //     2: split(3, 4)
        //     3: 'a'
        //     4: split(5, 6)
        //     5: 'a'
        //     6: split(7, 8)
        //     7: 'a'
        //     8: MATCH
        //
        // This is *incredibly* inefficient because the splits end
        // up forming a chain. Given a much larger number than `5`,
        // it is easy cause perverse behavior in the matching engines
        // like stack overflows. We could fix the matching engine,
        // but instead, we should just make the program smarter.
        // Thus, we do a custom job here and instead of chaining the
        // splits together, we simply point them to the MATCH
        // instruction directly (for example).
        let mut holes = vec![];
        let mut prev_hole = Hole::None;
        for _ in min..max {
            self.fill_to_next(prev_hole);
            let split = self.push_split_hole();
            let goto_rep_expr = self.insts.len();
            prev_hole = try!(self.c(expr));
            if greedy {
                holes.push(self.fill_split(split, Some(goto_rep_expr), None));
            } else {
                holes.push(self.fill_split(split, None, Some(goto_rep_expr)));
            }
        }
        holes.push(prev_hole);
        Ok(Hole::Many(holes))
    }

    fn fill(&mut self, hole: Hole, goto: InstIdx) {
        match hole {
            Hole::None => {}
            Hole::One(pc) => {
                self.insts[pc].fill(goto);
            }
            Hole::Many(holes) => {
                for hole in holes {
                    self.fill(hole, goto);
                }
            }
        }
    }

    fn fill_to_next(&mut self, hole: Hole) {
        let next = self.insts.len();
        self.fill(hole, next);
    }

    fn fill_split(
        &mut self,
        hole: Hole,
        goto1: Option<InstIdx>,
        goto2: Option<InstIdx>,
    ) -> Hole {
        match hole {
            Hole::None => Hole::None,
            Hole::One(pc) => {
                match (goto1, goto2) {
                    (Some(goto1), Some(goto2)) => {
                        self.insts[pc].fill_split(goto1, goto2);
                        Hole::None
                    }
                    (Some(goto1), None) => {
                        self.insts[pc].half_fill_split_goto1(goto1);
                        Hole::One(pc)
                    }
                    (None, Some(goto2)) => {
                        self.insts[pc].half_fill_split_goto2(goto2);
                        Hole::One(pc)
                    }
                    (None, None) => unreachable!("at least one of the split \
                                                  holes must be filled"),
                }
            }
            Hole::Many(holes) => {
                let mut new_holes = vec![];
                for hole in holes {
                    new_holes.push(self.fill_split(hole, goto1, goto2));
                }
                if new_holes.is_empty() {
                    Hole::None
                } else if new_holes.len() == 1 {
                    new_holes.pop().unwrap()
                } else {
                    Hole::Many(new_holes)
                }
            }
        }
    }

    fn push_compiled(&mut self, inst: Inst) {
        self.insts.push(MaybeInst::Compiled(inst));
    }

    fn push_hole(&mut self, inst: InstHole) -> Hole {
        let hole = self.insts.len();
        self.insts.push(MaybeInst::Uncompiled(inst));
        Hole::One(hole)
    }

    fn push_split_hole(&mut self) -> Hole {
        let hole = self.insts.len();
        self.insts.push(MaybeInst::Split);
        Hole::One(hole)
    }

    fn check_size(&self) -> Result<(), Error> {
        use std::mem::size_of;

        if self.insts.len() * size_of::<Inst>() > self.size_limit {
            Err(Error::CompiledTooBig(self.size_limit))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
enum Hole {
    None,
    One(InstIdx),
    Many(Vec<Hole>),
}

#[derive(Clone, Debug)]
enum MaybeInst {
    Compiled(Inst),
    Uncompiled(InstHole),
    Split,
    Split1(InstIdx),
    Split2(InstIdx),
}

impl MaybeInst {
    fn fill(&mut self, goto: InstIdx) {
        let filled = match *self {
            MaybeInst::Uncompiled(ref inst) => inst.fill(goto),
            MaybeInst::Split1(goto1) => {
                Inst::Split(InstSplit { goto1: goto1, goto2: goto })
            }
            MaybeInst::Split2(goto2) => {
                Inst::Split(InstSplit { goto1: goto, goto2: goto2 })
            }
            _ => unreachable!("not all instructions were compiled! \
                               found uncompiled instruction: {:?}", self),
        };
        *self = MaybeInst::Compiled(filled);
    }

    fn fill_split(&mut self, goto1: InstIdx, goto2: InstIdx) {
        let filled = match *self {
            MaybeInst::Split => {
                Inst::Split(InstSplit { goto1: goto1, goto2: goto2 })
            }
            _ => unreachable!("must be called on Split instruction, \
                               instead it was called on: {:?}", self),
        };
        *self = MaybeInst::Compiled(filled);
    }

    fn half_fill_split_goto1(&mut self, goto1: InstIdx) {
        let half_filled = match *self {
            MaybeInst::Split => goto1,
            _ => unreachable!("must be called on Split instruction, \
                               instead it was called on: {:?}", self),
        };
        *self = MaybeInst::Split1(half_filled);
    }

    fn half_fill_split_goto2(&mut self, goto2: InstIdx) {
        let half_filled = match *self {
            MaybeInst::Split => goto2,
            _ => unreachable!("must be called on Split instruction, \
                               instead it was called on: {:?}", self),
        };
        *self = MaybeInst::Split2(half_filled);
    }

    fn unwrap(self) -> Inst {
        match self {
            MaybeInst::Compiled(inst) => inst,
            _ => unreachable!("must be called on a compiled instruction, \
                               instead it was called on: {:?}", self),
        }
    }
}

#[derive(Clone, Debug)]
enum InstHole {
    Save { slot: usize },
    EmptyLook { look: EmptyLook },
    Char { c: char },
    Ranges { ranges: Vec<(char, char)> },
    Bytes { start: u8, end: u8 },
}

impl InstHole {
    fn fill(&self, goto: InstIdx) -> Inst {
        match *self {
            InstHole::Save { slot } => Inst::Save(InstSave {
                goto: goto,
                slot: slot,
            }),
            InstHole::EmptyLook { look } => Inst::EmptyLook(InstEmptyLook {
                goto: goto,
                look: look,
            }),
            InstHole::Char { c } => Inst::Char(InstChar {
                goto: goto,
                c: c,
            }),
            InstHole::Ranges { ref ranges } => Inst::Ranges(InstRanges {
                goto: goto,
                ranges: ranges.clone(),
            }),
            InstHole::Bytes { start, end } => Inst::Bytes(InstBytes {
                goto: goto,
                start: start,
                end: end,
            }),
        }
    }
}

struct CompileClass<'a, 'b> {
    c: &'a mut Compiler,
    ranges: &'b [ClassRange],
    suffix_cache: HashMap<SuffixCacheKey, InstIdx>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct SuffixCacheKey {
    from_inst: InstIdx,
    start: u8,
    end: u8,
}

impl<'a, 'b> CompileClass<'a, 'b> {
    fn compile(mut self) -> CompileResult {
        let mut holes = vec![];
        let mut it = self
            .ranges.iter()
            .flat_map(|r| Utf8Sequences::new(r.start, r.end))
            .peekable();
        let mut utf8_seq = it.next().expect("non-empty char class");
        let mut last_split = Hole::None;
        while it.peek().is_some() {
            self.c.fill_to_next(last_split);
            last_split = self.c.push_split_hole();
            holes.push(try!(self.c_utf8_sequence(&utf8_seq)));
            let goto1 = self.c.insts.len().checked_sub(1).unwrap();
            last_split = self.c.fill_split(last_split, Some(goto1), None);

            utf8_seq = it.next().unwrap();
        }
        holes.push(try!(self.c_utf8_sequence(&utf8_seq)));
        let goto1 = self.c.insts.len().checked_sub(1).unwrap();
        self.c.fill(last_split, goto1);
        Ok(Hole::Many(holes))
    }

    fn c_utf8_sequence(&mut self, seq: &Utf8Sequence) -> CompileResult {
        // The initial instruction for each UTF-8 sequence should be the same.
        // Since the 0th instruction is always `Save(0)`, it's safe to use it
        // as a sentinel here.
        let mut from_inst = 0;
        let mut last_hole = Hole::None;
        for byte_range in seq.into_iter().rev() {
            let key = SuffixCacheKey {
                from_inst: from_inst,
                start: byte_range.start,
                end: byte_range.end,
            };
            match self.suffix_cache.entry(key) {
                Entry::Occupied(e) => {
                    from_inst = *e.get();
                }
                Entry::Vacant(e) => {
                    if from_inst == 0 {
                        last_hole = self.c.push_hole(InstHole::Bytes {
                            start: byte_range.start,
                            end: byte_range.end,
                        });
                    } else {
                        self.c.push_compiled(Inst::Bytes(InstBytes {
                            goto: from_inst,
                            start: byte_range.start,
                            end: byte_range.end,
                        }));
                    }
                    from_inst = self.c.insts.len().checked_sub(1).unwrap();
                    e.insert(from_inst);
                }
            }
        }
        Ok(last_hole)
    }
}

struct CompileClassUncached<'a, 'b> {
    c: &'a mut Compiler,
    ranges: &'b [ClassRange],
}

impl<'a, 'b> CompileClassUncached<'a, 'b> {
    fn compile(mut self) -> CompileResult {
        let mut holes = vec![];
        let mut it = self
            .ranges.iter()
            .flat_map(|r| Utf8Sequences::new(r.start, r.end))
            .peekable();
        let mut utf8_seq = it.next().expect("non-empty char class");
        while it.peek().is_some() {
            let split = self.c.push_split_hole();
            let goto1 = self.c.insts.len();
            holes.push(try!(self.c_utf8_sequence(&utf8_seq)));
            let goto2 = self.c.insts.len();
            self.c.fill_split(split, Some(goto1), Some(goto2));

            utf8_seq = it.next().unwrap();
        }
        holes.push(try!(self.c_utf8_sequence(&utf8_seq)));
        Ok(Hole::Many(holes))
    }

    fn c_utf8_sequence(&mut self, seq: &Utf8Sequence) -> CompileResult {
        let mut prev_hole = Hole::None;
        for byte_range in seq {
            self.c.fill_to_next(prev_hole); // no-op on first iteration
            prev_hole = self.c.push_hole(InstHole::Bytes {
                start: byte_range.start,
                end: byte_range.end,
            });
        }
        Ok(prev_hole)
    }
}

fn u32_to_usize(n: u32) -> usize {
    if (n as u64) > (::std::usize::MAX as u64) {
        panic!("BUG: {} is too big to be pointer sized", n)
    }
    n as usize
}
