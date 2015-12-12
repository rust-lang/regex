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
use std::ops::{Deref, DerefMut, Drop};
use std::sync::Mutex;

/// A very simple memory pool for managing cached state.
///
/// This was motivated by a singular purpose: reduce the allocation overhead
/// of matching engines.
///
/// With a pool, the matching engines need to allocate state each time they
/// are invoked. If a regex is used once to check for a match and never again,
/// then this is OK. But if a regex is used many times over, then not
/// re-allocating the engine's state is a huge win. (A regex is commonly
/// used many times, for example, with `find_iter`, `captures_iter` or
/// `replace_all`.)
///
/// We use inherited mutability and ensure that each thread gets its own
/// state. There is no limit on the number of states that are created. If a
/// thread requests one and one isn't available, a new one is created.
pub struct Pool<T> {
    stack: Mutex<Vec<T>>,
    create: CreateFn<T>,
}

/// The type of the function used to create resources if none exist.
pub type CreateFn<T> = Box<Fn() -> T + Send + Sync>;

/// A guard the provides access to a value pulled from the pool.
#[derive(Debug)]
pub struct PoolGuard<'a, T: 'a> {
    pool: &'a Pool<T>,
    val: Option<T>,
}

impl<T> Pool<T> {
    /// Create a new pool.
    ///
    /// When a caller requests a resource from the pool and one does not
    /// exist, then `create` is called to allocate a new resource for the
    /// caller.
    ///
    /// It is up to the caller to put the resource back into the pool for
    /// future reuse.
    ///
    /// All resources are created lazily/on-demand.
    pub fn new(create: CreateFn<T>) -> Pool<T> {
        Pool {
            stack: Mutex::new(vec![]),
            create: create,
        }
    }

    /// Request a resource from the pool.
    ///
    /// If no resources are available, a new one is created.
    ///
    /// Once the guard is dropped, the resource is returned to the pool.
    pub fn get(&self) -> PoolGuard<T> {
        let mut stack = self.stack.lock().unwrap();
        match stack.pop() {
            None => PoolGuard { pool: self, val: Some((self.create)()) },
            Some(v) => PoolGuard { pool: self, val: Some(v) },
        }
    }

    /// Add a resource to the pool.
    ///
    /// This makes the resource available for use with `get`.
    fn put(&self, v: T) {
        let mut stack = self.stack.lock().unwrap();
        stack.push(v);
    }
}

impl<'a, T> Deref for PoolGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T { self.val.as_ref().unwrap() }
}

impl<'a, T> DerefMut for PoolGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T { self.val.as_mut().unwrap() }
}

impl<'a, T> Drop for PoolGuard<'a, T> {
    fn drop(&mut self) {
        let val = self.val.take().unwrap();
        self.pool.put(val);
    }
}

impl<T: fmt::Debug> fmt::Debug for Pool<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let stack = self.stack.lock();
        let stack = stack.unwrap();
        stack.fmt(f)
    }
}
