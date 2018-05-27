// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
use std::cell::RefCell;
use std::vec::IntoIter;

use hyperscan::Error;
use hyperscan::{BlockDatabase, BlockScanner, DatabaseBuilder, HS_CPU_FEATURES_AVX2, Pattern,
                RawScratch, ScratchAllocator, HS_FLAG_ALLOWEMPTY, HS_FLAG_SINGLEMATCH,
                HS_FLAG_SOM_LEFTMOST};

/// Regex wraps a hyperscan regular expression.
///
/// It can be used safely from multiple threads simultaneously.
pub struct Regex {
    db: BlockDatabase,
    s: RefCell<RawScratch>,
}

unsafe impl Send for Regex {}

impl Regex {
    pub fn new(pattern: &str) -> Result<Regex, Error> {
        let mut p = pattern.parse::<Pattern>()?;
        p.flags.set(HS_FLAG_ALLOWEMPTY | HS_CPU_FEATURES_AVX2);
        let db = p.build()?;
        let s = RefCell::new(db.alloc()?);

        Ok(Regex { db, s })
    }

    pub fn is_match(&self, text: &str) -> bool {
        self.scan(text)
            .ok()
            .map_or(false, |matched| !matched.is_empty())
    }

    pub fn find_iter<'a>(&'a self, text: &str) -> FindMatches {
        self.scan(text).ok().unwrap().into_iter()
    }

    fn scan(&self, text: &str) -> Result<Vec<(usize, usize)>, Error> {
        let matches = RefCell::new(Vec::<(usize, usize)>::new());

        self.db.scan(
            text,
            0,
            &mut *self.s.borrow_mut(),
            Some(on_matched),
            Some(&matches),
        )?;

        Ok(matches.into_inner())
    }
}

pub type FindMatches = IntoIter<(usize, usize)>;

fn on_matched(
    _id: u32,
    from: u64,
    to: u64,
    _flags: u32,
    matches: &RefCell<Vec<(usize, usize)>>,
) -> u32 {
    matches.borrow_mut().push((from as usize, to as usize));
    0
}
