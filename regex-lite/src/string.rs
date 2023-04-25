use alloc::{
    borrow::Cow, boxed::Box, string::String, string::ToString, sync::Arc, vec,
    vec::Vec,
};

use crate::{
    error::Error,
    hir::{self, Hir},
    int::NonMaxUsize,
    interpolate,
    nfa::{self, NFA},
    pikevm::{self, Cache, PikeVM},
    pool::CachePool,
};

#[derive(Debug)]
pub struct Regex {
    pikevm: Arc<PikeVM>,
    pool: CachePool,
}

impl Regex {
    pub fn new(pattern: &str) -> Result<Regex, Error> {
        RegexBuilder::new(pattern).build()
    }

    pub fn is_match(&self, haystack: &str) -> bool {
        self.is_match_at(haystack, 0)
    }

    pub fn find<'h>(&self, haystack: &'h str) -> Option<Match<'h>> {
        self.find_at(haystack, 0)
    }

    pub fn find_iter<'r, 'h>(&'r self, haystack: &'h str) -> Matches<'r, 'h> {
        Matches {
            haystack,
            it: self.pikevm.find_iter(self.pool.get(), haystack.as_bytes()),
        }
    }

    pub fn captures<'h>(&self, haystack: &'h str) -> Option<Captures<'h>> {
        self.captures_at(haystack, 0)
    }

    pub fn captures_iter<'r, 'h>(
        &'r self,
        haystack: &'h str,
    ) -> CaptureMatches<'r, 'h> {
        CaptureMatches {
            haystack,
            re: self,
            it: self
                .pikevm
                .captures_iter(self.pool.get(), haystack.as_bytes()),
        }
    }

    pub fn split<'r, 'h>(&'r self, haystack: &'h str) -> Split<'r, 'h> {
        Split { haystack, finder: self.find_iter(haystack), last: 0 }
    }

    pub fn splitn<'r, 'h>(
        &'r self,
        haystack: &'h str,
        limit: usize,
    ) -> SplitN<'r, 'h> {
        SplitN { splits: self.split(haystack), limit }
    }

    pub fn replace<'h, R: Replacer>(
        &self,
        haystack: &'h str,
        rep: R,
    ) -> Cow<'h, str> {
        self.replacen(haystack, 1, rep)
    }

    pub fn replace_all<'h, R: Replacer>(
        &self,
        haystack: &'h str,
        rep: R,
    ) -> Cow<'h, str> {
        self.replacen(haystack, 0, rep)
    }

    pub fn replacen<'h, R: Replacer>(
        &self,
        haystack: &'h str,
        limit: usize,
        mut rep: R,
    ) -> Cow<'h, str> {
        // If we know that the replacement doesn't have any capture expansions,
        // then we can use the fast path. The fast path can make a tremendous
        // difference:
        //
        //   1) We use `find_iter` instead of `captures_iter`. Not asking for
        //      captures generally makes the regex engines faster.
        //   2) We don't need to look up all of the capture groups and do
        //      replacements inside the replacement string. We just push it
        //      at each match and be done with it.
        if let Some(rep) = rep.no_expansion() {
            let mut it = self.find_iter(haystack).enumerate().peekable();
            if it.peek().is_none() {
                return Cow::Borrowed(haystack);
            }
            let mut new = String::with_capacity(haystack.len());
            let mut last_match = 0;
            for (i, m) in it {
                new.push_str(&haystack[last_match..m.start()]);
                new.push_str(&rep);
                last_match = m.end();
                if limit > 0 && i >= limit - 1 {
                    break;
                }
            }
            new.push_str(&haystack[last_match..]);
            return Cow::Owned(new);
        }

        // The slower path, which we use if the replacement needs access to
        // capture groups.
        let mut it = self.captures_iter(haystack).enumerate().peekable();
        if it.peek().is_none() {
            return Cow::Borrowed(haystack);
        }
        let mut new = String::with_capacity(haystack.len());
        let mut last_match = 0;
        for (i, cap) in it {
            // unwrap on 0 is OK because captures only reports matches
            let m = cap.get(0).unwrap();
            new.push_str(&haystack[last_match..m.start()]);
            rep.replace_append(&cap, &mut new);
            last_match = m.end();
            if limit > 0 && i >= limit - 1 {
                break;
            }
        }
        new.push_str(&haystack[last_match..]);
        Cow::Owned(new)
    }
}

/// Advanced or "lower level" search methods.
impl Regex {
    /// Returns the end location of a match in the haystack given.
    ///
    /// This method may have the same performance characteristics as
    /// `is_match`, except it provides an end location for a match. In
    /// particular, the location returned *may be shorter* than the proper end
    /// of the leftmost-first match that you would find via `Regex::find`.
    ///
    /// Note that it is not guaranteed that this routine finds the shortest or
    /// "earliest" possible match. Instead, the main idea of this API is that
    /// it returns the offset at the point at which the internal regex engine
    /// has determined that a match has occurred. This may vary depending on
    /// which internal regex engine is used, and thus, the offset itself may
    /// change.
    ///
    /// # Example
    ///
    /// Typically, `a+` would match the entire first sequence of `a` in some
    /// haystack, but `shortest_match` can give up as soon as it sees the first
    /// `a`.
    ///
    /// ```
    ///
    /// use regex_lite::Regex;
    /// let haystack = "aaaaa";
    /// let pos = Regex::new(r"a+").unwrap().shortest_match(haystack);
    /// assert_eq!(pos, Some(1));
    /// ```
    pub fn shortest_match(&self, haystack: &str) -> Option<usize> {
        self.shortest_match_at(haystack, 0)
    }

    /// Returns the same as `shortest_match`, but starts the search at the
    /// given offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only match
    /// when `start == 0`.
    pub fn shortest_match_at(
        &self,
        haystack: &str,
        start: usize,
    ) -> Option<usize> {
        let mut cache = self.pool.get();
        let mut slots = [None, None];
        let matched = self.pikevm.search(
            &mut cache,
            haystack.as_bytes(),
            start,
            haystack.len(),
            true,
            &mut slots,
        );
        if !matched {
            return None;
        }
        Some(slots[1].unwrap().get())
    }

    /// Returns the same as is_match, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    pub fn is_match_at(&self, haystack: &str, start: usize) -> bool {
        let mut cache = self.pool.get();
        self.pikevm.search(
            &mut cache,
            haystack.as_bytes(),
            start,
            haystack.len(),
            true,
            &mut [],
        )
    }

    /// Returns the same as find, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    pub fn find_at<'h>(
        &self,
        haystack: &'h str,
        start: usize,
    ) -> Option<Match<'h>> {
        let mut cache = self.pool.get();
        let mut slots = [None, None];
        let matched = self.pikevm.search(
            &mut cache,
            haystack.as_bytes(),
            start,
            haystack.len(),
            false,
            &mut slots,
        );
        if !matched {
            return None;
        }
        let (start, end) = (slots[0].unwrap().get(), slots[1].unwrap().get());
        Some(Match::new(haystack, start, end))
    }

    /// Returns the same as [`Regex::captures`], but starts the search at the
    /// given offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    #[inline]
    pub fn captures_at<'h>(
        &self,
        haystack: &'h str,
        start: usize,
    ) -> Option<Captures<'h>> {
        let mut caps = Captures {
            haystack,
            slots: self.capture_locations(),
            pikevm: Arc::clone(&self.pikevm),
        };
        let mut cache = self.pool.get();
        let matched = self.pikevm.search(
            &mut cache,
            haystack.as_bytes(),
            start,
            haystack.len(),
            false,
            &mut caps.slots.0,
        );
        if !matched {
            return None;
        }
        Some(caps)
    }

    /// This is like `captures`, but uses
    /// [`CaptureLocations`](struct.CaptureLocations.html)
    /// instead of
    /// [`Captures`](struct.Captures.html) in order to amortize allocations.
    ///
    /// To create a `CaptureLocations` value, use the
    /// `Regex::capture_locations` method.
    ///
    /// This returns the overall match if this was successful, which is always
    /// equivalence to the `0`th capture group.
    #[inline]
    pub fn captures_read<'h>(
        &self,
        locs: &mut CaptureLocations,
        haystack: &'h str,
    ) -> Option<Match<'h>> {
        self.captures_read_at(locs, haystack, 0)
    }

    /// Returns the same as captures, but starts the search at the given
    /// offset and populates the capture locations given.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    #[inline]
    pub fn captures_read_at<'h>(
        &self,
        locs: &mut CaptureLocations,
        haystack: &'h str,
        start: usize,
    ) -> Option<Match<'h>> {
        let mut cache = self.pool.get();
        let matched = self.pikevm.search(
            &mut cache,
            haystack.as_bytes(),
            start,
            haystack.len(),
            false,
            &mut locs.0,
        );
        if !matched {
            return None;
        }
        let (start, end) = locs.get(0).unwrap();
        Some(Match::new(haystack, start, end))
    }

    /// An undocumented alias for `captures_read_at`.
    ///
    /// The `regex-capi` crate previously used this routine, so to avoid
    /// breaking that crate, we continue to provide the name as an undocumented
    /// alias.
    #[doc(hidden)]
    #[inline]
    pub fn read_captures_at<'h>(
        &self,
        locs: &mut CaptureLocations,
        haystack: &'h str,
        start: usize,
    ) -> Option<Match<'h>> {
        self.captures_read_at(locs, haystack, start)
    }
}

/// Auxiliary methods.
impl Regex {
    /// Returns the original string of this regex.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.pikevm.nfa().pattern()
    }

    /// Returns an iterator over the capture names.
    pub fn capture_names(&self) -> CaptureNames<'_> {
        CaptureNames(self.pikevm.nfa().capture_names())
    }

    /// Returns the number of captures.
    pub fn captures_len(&self) -> usize {
        self.pikevm.nfa().group_len()
    }

    /// Returns an empty set of capture locations that can be reused in
    /// multiple calls to `captures_read` or `captures_read_at`.
    #[inline]
    pub fn capture_locations(&self) -> CaptureLocations {
        // OK because NFA construction would have failed if this overflowed.
        let len = self.pikevm.nfa().group_len().checked_mul(2).unwrap();
        CaptureLocations(vec![None; len])
    }

    /// An alias for `capture_locations` to preserve backward compatibility.
    ///
    /// The `regex-capi` crate uses this method, so to avoid breaking that
    /// crate, we continue to export it as an undocumented API.
    #[doc(hidden)]
    #[inline]
    pub fn locations(&self) -> CaptureLocations {
        self.capture_locations()
    }
}

impl Clone for Regex {
    fn clone(&self) -> Regex {
        let pikevm = Arc::clone(&self.pikevm);
        let pool = {
            let pikevm = Arc::clone(&self.pikevm);
            let create = Box::new(move || Cache::new(&pikevm));
            CachePool::new(create)
        };
        Regex { pikevm, pool }
    }
}

/// Match represents a single match of a regex in a haystack.
///
/// The lifetime parameter `'h` refers to the lifetime of the haystack.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Match<'h> {
    haystack: &'h str,
    start: usize,
    end: usize,
}

impl<'h> Match<'h> {
    /// Creates a new match from the given haystack and byte offsets.
    fn new(haystack: &'h str, start: usize, end: usize) -> Match<'h> {
        Match { haystack, start, end }
    }

    /// Returns the starting byte offset of the match in the haystack.
    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the ending byte offset of the match in the haystack.
    #[inline]
    pub fn end(&self) -> usize {
        self.end
    }

    /// Returns true if and only if this match has a length of zero.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Returns the length, in bytes, of this match.
    #[inline]
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns the range over the starting and ending byte offsets of the
    /// match in the haystack.
    #[inline]
    pub fn range(&self) -> core::ops::Range<usize> {
        self.start..self.end
    }

    /// Returns the matched portion of the haystack.
    #[inline]
    pub fn as_str(&self) -> &'h str {
        &self.haystack[self.range()]
    }
}

impl<'h> std::fmt::Debug for Match<'h> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Match")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("haystack", &self.as_str())
            .finish()
    }
}

impl<'h> From<Match<'h>> for &'h str {
    fn from(m: Match<'h>) -> &'h str {
        m.as_str()
    }
}

impl<'h> From<Match<'h>> for core::ops::Range<usize> {
    fn from(m: Match<'h>) -> core::ops::Range<usize> {
        m.range()
    }
}

/// Captures represents a group of captured strings for a single match.
///
/// The 0th capture always corresponds to the entire match. Each subsequent
/// index corresponds to the next capture group in the regex. If a capture
/// group is named, then the matched string is *also* available via the `name`
/// method. (Note that the 0th capture is always unnamed and so must be
/// accessed with the `get` method.)
///
/// Positions returned from a capture group are always byte indices.
///
/// `'h` is the lifetime of the matched haystack.
pub struct Captures<'h> {
    haystack: &'h str,
    slots: CaptureLocations,
    // It's a little weird to put the PikeVM in our Captures, but it's the
    // simplest thing to do and is cheap. The PikeVM gives us access to the
    // NFA and the NFA gives us access to the capture name<->index mapping.
    pikevm: Arc<PikeVM>,
}

impl<'h> Captures<'h> {
    /// Returns the match associated with the capture group at index `i`. If
    /// `i` does not correspond to a capture group, or if the capture group
    /// did not participate in the match, then `None` is returned.
    ///
    /// # Examples
    ///
    /// Get the haystack of the match with a default of an empty string if this
    /// group didn't participate in the match:
    ///
    /// ```rust
    /// use regex_lite::Regex;
    ///
    /// let re = Regex::new(r"[a-z]+(?:([0-9]+)|([A-Z]+))").unwrap();
    /// let caps = re.captures("abc123").unwrap();
    ///
    /// let hay1 = caps.get(1).map_or("", |m| m.as_str());
    /// let hay2 = caps.get(2).map_or("", |m| m.as_str());
    /// assert_eq!(hay1, "123");
    /// assert_eq!(hay2, "");
    /// ```
    #[inline]
    pub fn get(&self, i: usize) -> Option<Match<'h>> {
        self.slots.get(i).map(|(s, e)| Match::new(self.haystack, s, e))
    }

    /// Returns the match for the capture group named `name`. If `name` isn't a
    /// valid capture group or didn't match anything, then `None` is returned.
    #[inline]
    pub fn name(&self, name: &str) -> Option<Match<'h>> {
        let i = self.pikevm.nfa().to_index(name)?;
        self.get(i)
    }

    /// An iterator that yields all capturing matches in the order in which
    /// they appear in the regex. If a particular capture group didn't
    /// participate in the match, then `None` is yielded for that capture.
    ///
    /// The first match always corresponds to the overall match of the regex.
    #[inline]
    pub fn iter<'c>(&'c self) -> SubCaptureMatches<'c, 'h> {
        SubCaptureMatches {
            caps: self,
            it: self.pikevm.nfa().capture_names().enumerate(),
        }
    }

    /// Expands all instances of `$name` in `replacement` to the corresponding
    /// capture group `name`, and writes them to the `dst` buffer given.
    ///
    /// `name` may be an integer corresponding to the index of the capture
    /// group (counted by order of opening parenthesis where `0` is the
    /// entire match) or it can be a name (consisting of letters, digits or
    /// underscores) corresponding to a named capture group.
    ///
    /// If `name` isn't a valid capture group (whether the name doesn't exist
    /// or isn't a valid index), then it is replaced with the empty string.
    ///
    /// The longest possible name consisting of the characters `[_0-9A-Za-z]`
    /// is used. e.g., `$1a` looks up the capture group named `1a` and not the
    /// capture group at index `1`. To exert more precise control over the
    /// name, or to refer to a capture group name that uses characters outside
    /// of `[_0-9A-Za-z]`, use braces, e.g., `${1}a` or `${foo[bar].baz}`. When
    /// using braces, any sequence of characters is permitted. If the sequence
    /// does not refer to a capture group name in the corresponding regex, then
    /// it is replaced with an empty string.
    ///
    /// To write a literal `$` use `$$`.
    #[inline]
    pub fn expand(&self, replacement: &str, dst: &mut String) {
        interpolate::string(
            replacement,
            |index, dst| {
                let m = match self.get(index) {
                    None => return,
                    Some(m) => m,
                };
                dst.push_str(&self.haystack[m.range()]);
            },
            |name| self.pikevm.nfa().to_index(name),
            dst,
        );
    }

    /// Returns the total number of capture groups (even if they didn't match).
    ///
    /// This is always at least `1`, since every regex has at least one capture
    /// group that corresponds to the full match.
    #[inline]
    pub fn len(&self) -> usize {
        self.pikevm.nfa().group_len()
    }
}

impl<'h> core::fmt::Debug for Captures<'h> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // TODO: Make this better.
        f.debug_tuple("Captures").field(&self.slots).finish()
    }
}

/// Get a group by index.
///
/// `'h` is the lifetime of the matched portion of the haystack.
///
/// The haystack can't outlive the `Captures` object if this method is used,
/// because of how `Index` is defined (normally `a[i]` is part of `a` and can't
/// outlive it); to do that, use `get()` instead.
///
/// # Panics
///
/// If there is no group at the given index.
impl<'h> core::ops::Index<usize> for Captures<'h> {
    type Output = str;

    fn index(&self, i: usize) -> &str {
        self.get(i)
            .map(|m| m.as_str())
            .unwrap_or_else(|| panic!("no group at index '{}'", i))
    }
}

/// Get a group by name.
///
/// `'h` is the lifetime of the matched portion of the haystack and `'n` is the
/// lifetime of the group name that is used as the lookup key.
///
/// The haystack can't outlive the `Captures` object if this method is used,
/// because of how `Index` is defined (normally `a[i]` is part of `a` and can't
/// outlive it); to do that, use `name` instead.
///
/// # Panics
///
/// If there is no group named by the given value.
impl<'h, 'n> core::ops::Index<&'n str> for Captures<'h> {
    type Output = str;

    fn index<'a>(&'a self, name: &'n str) -> &'a str {
        self.name(name)
            .map(|m| m.as_str())
            .unwrap_or_else(|| panic!("no group named '{}'", name))
    }
}

/// An iterator that yields all capturing matches in the order in which they
/// appear in the regex.
///
/// If a particular capture group didn't participate in the match, then `None`
/// is yielded for that capture. The first match always corresponds to the
/// overall match of the regex.
///
/// The lifetime `'c` corresponds to the lifetime of the `Captures` value, and
/// the lifetime `'h` corresponds to the originally matched haystack.
#[derive(Clone, Debug)]
pub struct SubCaptureMatches<'c, 'h> {
    caps: &'c Captures<'h>,
    it: core::iter::Enumerate<nfa::CaptureNames<'c>>,
}

impl<'c, 'h> Iterator for SubCaptureMatches<'c, 'h> {
    type Item = Option<Match<'h>>;

    #[inline]
    fn next(&mut self) -> Option<Option<Match<'h>>> {
        let (group_index, _) = self.it.next()?;
        Some(self.caps.get(group_index))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.it.size_hint()
    }

    #[inline]
    fn count(self) -> usize {
        self.it.count()
    }
}

impl<'c, 'h> ExactSizeIterator for SubCaptureMatches<'c, 'h> {}

impl<'c, 'h> core::iter::FusedIterator for SubCaptureMatches<'c, 'h> {}

/// CaptureLocations is a low level representation of the raw offsets of each
/// submatch.
///
/// You can think of this as a lower level [`Captures`], where this type does
/// not support named capturing groups directly and it does not borrow the
/// haystack that these offsets were matched on.
///
/// Primarily, this type is useful when using the lower level `Regex` APIs
/// such as `read_captures`, which permits amortizing the allocation in which
/// capture match locations are stored.
///
/// In order to build a value of this type, you'll need to call the
/// `capture_locations` method on the `Regex` being used to execute the search.
/// The value returned can then be reused in subsequent searches.
///
/// # Example
///
/// This example shows how to create and use `CaptureLocations` in a search.
///
/// ```
/// use regex_lite::Regex;
///
/// let re = Regex::new(r"(?<first>\w+)\s+(?<last>\w+)").unwrap();
/// let mut locs = re.capture_locations();
/// let m = re.captures_read(&mut locs, "Bruce Springsteen").unwrap();
/// assert_eq!(0..17, m.range());
/// assert_eq!(Some((0, 17)), locs.get(0));
/// assert_eq!(Some((0, 5)), locs.get(1));
/// assert_eq!(Some((6, 17)), locs.get(2));
///
/// // Asking for an invalid capture group always returns None.
/// assert_eq!(None, locs.get(3));
/// assert_eq!(None, locs.get(34973498648));
/// assert_eq!(None, locs.get(9944060567225171988));
/// ```
#[derive(Clone, Debug)]
pub struct CaptureLocations(Vec<Option<NonMaxUsize>>);

/// A type alias for `CaptureLocations` for backwards compatibility.
///
/// Previously, we exported `CaptureLocations` as `Locations` in an
/// undocumented API. To prevent breaking that code (e.g., in `regex-capi`),
/// we continue re-exporting the same undocumented API.
#[doc(hidden)]
pub type Locations = CaptureLocations;

impl CaptureLocations {
    /// Returns the start and end positions of the Nth capture group. Returns
    /// `None` if `i` is not a valid capture group or if the capture group did
    /// not match anything. The positions returned are *always* byte indices
    /// with respect to the original string matched.
    #[inline]
    pub fn get(&self, i: usize) -> Option<(usize, usize)> {
        let slot = i.checked_mul(2)?;
        let start = self.0.get(slot).copied()??.get();
        let slot = slot.checked_add(1)?;
        let end = self.0.get(slot).copied()??.get();
        Some((start, end))
    }

    /// Returns the total number of capture groups (even if they didn't match).
    ///
    /// This is always at least `1` since every regex has at least `1`
    /// capturing group that corresponds to the entire match.
    #[inline]
    pub fn len(&self) -> usize {
        // We always have twice as many slots as groups.
        self.0.len().checked_shr(1).unwrap()
    }

    /// An alias for the `get` method for backwards compatibility.
    ///
    /// Previously, we exported `get` as `pos` in an undocumented API. To
    /// prevent breaking that code (e.g., in `regex-capi`), we continue
    /// re-exporting the same undocumented API.
    #[doc(hidden)]
    #[inline]
    pub fn pos(&self, i: usize) -> Option<(usize, usize)> {
        self.get(i)
    }
}

/// An iterator over the names of all possible captures.
///
/// `None` indicates an unnamed capture; the first element (capture 0, the
/// whole matched region) is always unnamed.
///
/// `'r` is the lifetime of the compiled regular expression.
#[derive(Clone, Debug)]
pub struct CaptureNames<'r>(nfa::CaptureNames<'r>);

impl<'r> Iterator for CaptureNames<'r> {
    type Item = Option<&'r str>;

    #[inline]
    fn next(&mut self) -> Option<Option<&'r str>> {
        self.0.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }

    #[inline]
    fn count(self) -> usize {
        self.0.count()
    }
}

impl<'r> ExactSizeIterator for CaptureNames<'r> {}

impl<'r> core::iter::FusedIterator for CaptureNames<'r> {}

/// An iterator over all non-overlapping matches for a particular string.
///
/// The iterator yields a `Match` value. The iterator stops when no more
/// matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'h` is the
/// lifetime of the matched string.
#[derive(Debug)]
pub struct Matches<'r, 'h> {
    haystack: &'h str,
    it: pikevm::FindMatches<'r, 'h>,
}

impl<'r, 'h> Iterator for Matches<'r, 'h> {
    type Item = Match<'h>;

    #[inline]
    fn next(&mut self) -> Option<Match<'h>> {
        self.it.next().map(|(s, e)| Match::new(self.haystack, s, e))
    }

    #[inline]
    fn count(self) -> usize {
        self.it.count()
    }
}

impl<'r, 'h> core::iter::FusedIterator for Matches<'r, 'h> {}

/// An iterator that yields all non-overlapping capture groups matching a
/// particular regular expression.
///
/// The iterator stops when no more matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'h` is the
/// lifetime of the matched string.
#[derive(Debug)]
pub struct CaptureMatches<'r, 'h> {
    haystack: &'h str,
    re: &'r Regex,
    it: pikevm::CapturesMatches<'r, 'h>,
}

impl<'r, 'h> Iterator for CaptureMatches<'r, 'h> {
    type Item = Captures<'h>;

    #[inline]
    fn next(&mut self) -> Option<Captures<'h>> {
        self.it.next().map(|slots| Captures {
            haystack: self.haystack,
            slots: CaptureLocations(slots),
            pikevm: Arc::clone(&self.re.pikevm),
        })
    }

    #[inline]
    fn count(self) -> usize {
        self.it.count()
    }
}

impl<'r, 'h> core::iter::FusedIterator for CaptureMatches<'r, 'h> {}

/// Yields all substrings delimited by a regular expression match.
///
/// `'r` is the lifetime of the compiled regular expression and `'h` is the
/// lifetime of the string being split.
#[derive(Debug)]
pub struct Split<'r, 'h> {
    haystack: &'h str,
    finder: Matches<'r, 'h>,
    last: usize,
}

impl<'r, 'h> Iterator for Split<'r, 'h> {
    type Item = &'h str;

    #[inline]
    fn next(&mut self) -> Option<&'h str> {
        match self.finder.next() {
            None => {
                let len = self.haystack.len();
                if self.last > len {
                    None
                } else {
                    let range = self.last..len;
                    self.last = len + 1; // Next call will return None
                    Some(&self.haystack[range])
                }
            }
            Some(m) => {
                let range = m.range();
                self.last = m.end();
                Some(&self.haystack[range])
            }
        }
    }
}

impl<'r, 't> core::iter::FusedIterator for Split<'r, 't> {}

/// Yields at most `N` substrings delimited by a regular expression match.
///
/// The last substring will be whatever remains after splitting.
///
/// `'r` is the lifetime of the compiled regular expression and `'h` is the
/// lifetime of the string being split.
#[derive(Debug)]
pub struct SplitN<'r, 'h> {
    splits: Split<'r, 'h>,
    limit: usize,
}

impl<'r, 'h> Iterator for SplitN<'r, 'h> {
    type Item = &'h str;

    #[inline]
    fn next(&mut self) -> Option<&'h str> {
        if self.limit == 0 {
            return None;
        }

        self.limit -= 1;
        if self.limit > 0 {
            return self.splits.next();
        }

        let len = self.splits.haystack.len();
        if self.splits.last > len {
            // We've already returned all substrings.
            None
        } else {
            // self.n == 0, so future calls will return None immediately
            Some(&self.splits.haystack[self.splits.last..len])
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.splits.size_hint()
    }
}

impl<'r, 't> core::iter::FusedIterator for SplitN<'r, 't> {}

/// Replacer describes types that can be used to replace matches in a string.
///
/// In general, users of this crate shouldn't need to implement this trait,
/// since implementations are already provided for `&str` along with other
/// variants of string types and `FnMut(&Captures) -> String` (or any
/// `FnMut(&Captures) -> T` where `T: AsRef<str>`), which covers most use cases.
pub trait Replacer {
    /// Appends text to `dst` to replace the current match.
    ///
    /// The current match is represented by `caps`, which is guaranteed to
    /// have a match at capture group `0`.
    ///
    /// For example, a no-op replacement would be
    /// `dst.push_str(caps.get(0).unwrap().as_str())`.
    fn replace_append(&mut self, caps: &Captures<'_>, dst: &mut String);

    /// Return a fixed unchanging replacement string.
    ///
    /// When doing replacements, if access to `Captures` is not needed (e.g.,
    /// the replacement byte string does not need `$` expansion), then it can
    /// be beneficial to avoid finding sub-captures.
    ///
    /// In general, this is called once for every call to `replacen`.
    fn no_expansion<'r>(&'r mut self) -> Option<Cow<'r, str>> {
        None
    }

    /// Return a `Replacer` that borrows and wraps this `Replacer`.
    ///
    /// This is useful when you want to take a generic `Replacer` (which might
    /// not be cloneable) and use it without consuming it, so it can be used
    /// more than once.
    ///
    /// # Example
    ///
    /// ```
    /// use regex_lite::{Regex, Replacer};
    ///
    /// fn replace_all_twice<R: Replacer>(
    ///     re: Regex,
    ///     src: &str,
    ///     mut rep: R,
    /// ) -> String {
    ///     let dst = re.replace_all(src, rep.by_ref());
    ///     let dst = re.replace_all(&dst, rep.by_ref());
    ///     dst.into_owned()
    /// }
    /// ```
    fn by_ref<'r>(&'r mut self) -> ReplacerRef<'r, Self> {
        ReplacerRef(self)
    }
}

/// By-reference adaptor for a `Replacer`
///
/// Returned by [`Replacer::by_ref`](trait.Replacer.html#method.by_ref).
#[derive(Debug)]
pub struct ReplacerRef<'a, R: ?Sized>(&'a mut R);

impl<'a, R: Replacer + ?Sized + 'a> Replacer for ReplacerRef<'a, R> {
    fn replace_append(&mut self, caps: &Captures<'_>, dst: &mut String) {
        self.0.replace_append(caps, dst)
    }
    fn no_expansion(&mut self) -> Option<Cow<'_, str>> {
        self.0.no_expansion()
    }
}

impl<'a> Replacer for &'a str {
    fn replace_append(&mut self, caps: &Captures<'_>, dst: &mut String) {
        caps.expand(*self, dst);
    }

    fn no_expansion(&mut self) -> Option<Cow<'_, str>> {
        no_expansion(self)
    }
}

impl<'a> Replacer for &'a String {
    fn replace_append(&mut self, caps: &Captures<'_>, dst: &mut String) {
        self.as_str().replace_append(caps, dst)
    }

    fn no_expansion(&mut self) -> Option<Cow<'_, str>> {
        no_expansion(self)
    }
}

impl Replacer for String {
    fn replace_append(&mut self, caps: &Captures<'_>, dst: &mut String) {
        self.as_str().replace_append(caps, dst)
    }

    fn no_expansion(&mut self) -> Option<Cow<'_, str>> {
        no_expansion(self)
    }
}

impl<'a> Replacer for Cow<'a, str> {
    fn replace_append(&mut self, caps: &Captures<'_>, dst: &mut String) {
        self.as_ref().replace_append(caps, dst)
    }

    fn no_expansion(&mut self) -> Option<Cow<'_, str>> {
        no_expansion(self)
    }
}

impl<'a> Replacer for &'a Cow<'a, str> {
    fn replace_append(&mut self, caps: &Captures<'_>, dst: &mut String) {
        self.as_ref().replace_append(caps, dst)
    }

    fn no_expansion(&mut self) -> Option<Cow<'_, str>> {
        no_expansion(self)
    }
}

fn no_expansion<T: AsRef<str>>(t: &T) -> Option<Cow<'_, str>> {
    let s = t.as_ref();
    match s.find('$') {
        Some(_) => None,
        None => Some(Cow::Borrowed(s)),
    }
}

impl<F, T> Replacer for F
where
    F: FnMut(&Captures<'_>) -> T,
    T: AsRef<str>,
{
    fn replace_append(&mut self, caps: &Captures<'_>, dst: &mut String) {
        dst.push_str((*self)(caps).as_ref());
    }
}

/// `NoExpand` indicates literal string replacement.
///
/// It can be used with `replace` and `replace_all` to do a literal string
/// replacement without expanding `$name` to their corresponding capture
/// groups. This can be both convenient (to avoid escaping `$`, for example)
/// and performant (since capture groups don't need to be found).
///
/// `'t` is the lifetime of the literal text.
#[derive(Clone, Debug)]
pub struct NoExpand<'t>(pub &'t str);

impl<'t> Replacer for NoExpand<'t> {
    fn replace_append(&mut self, _: &Captures<'_>, dst: &mut String) {
        dst.push_str(self.0);
    }

    fn no_expansion(&mut self) -> Option<Cow<'_, str>> {
        Some(Cow::Borrowed(self.0))
    }
}

#[derive(Debug)]
pub struct RegexBuilder {
    pattern: String,
    hir_config: hir::Config,
    nfa_config: nfa::Config,
}

impl RegexBuilder {
    pub fn new(pattern: &str) -> RegexBuilder {
        RegexBuilder {
            pattern: pattern.to_string(),
            hir_config: hir::Config::default(),
            nfa_config: nfa::Config::default(),
        }
    }

    pub fn build(&self) -> Result<Regex, Error> {
        let hir = Hir::parse(self.hir_config, &self.pattern)?;
        let nfa = NFA::new(self.nfa_config, self.pattern.clone(), &hir)?;
        let pikevm = Arc::new(PikeVM::new(nfa));
        let pool = {
            let pikevm = Arc::clone(&pikevm);
            let create = Box::new(move || Cache::new(&pikevm));
            CachePool::new(create)
        };
        Ok(Regex { pikevm, pool })
    }

    pub fn case_insensitive(&mut self, yes: bool) -> &mut RegexBuilder {
        self.hir_config.flags.case_insensitive = yes;
        self
    }

    pub fn multi_line(&mut self, yes: bool) -> &mut RegexBuilder {
        self.hir_config.flags.multi_line = yes;
        self
    }

    pub fn crlf(&mut self, yes: bool) -> &mut RegexBuilder {
        self.hir_config.flags.crlf = yes;
        self
    }

    pub fn dot_matches_new_line(&mut self, yes: bool) -> &mut RegexBuilder {
        self.hir_config.flags.dot_matches_new_line = yes;
        self
    }

    pub fn swap_greed(&mut self, yes: bool) -> &mut RegexBuilder {
        self.hir_config.flags.swap_greed = yes;
        self
    }

    pub fn ignore_whitespace(&mut self, yes: bool) -> &mut RegexBuilder {
        self.hir_config.flags.ignore_whitespace = yes;
        self
    }

    pub fn size_limit(&mut self, limit: usize) -> &mut RegexBuilder {
        self.nfa_config.size_limit = Some(limit);
        self
    }

    pub fn nest_limit(&mut self, limit: u32) -> &mut RegexBuilder {
        self.hir_config.nest_limit = limit;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scratch() {
        let re = Regex::new("abc").unwrap();
        assert_eq!(Some(0..3), re.find("abc").map(|m| m.range()));

        let re = Regex::new("abc").unwrap();
        assert_eq!(Some(4..7), re.find("foo abc").map(|m| m.range()));

        let re = Regex::new("^abc").unwrap();
        assert_eq!(Some(0..3), re.find("abc").map(|m| m.range()));

        let re = Regex::new("^abc").unwrap();
        assert_eq!(None, re.find("foo abc").map(|m| m.range()));

        let re = Regex::new("(?Rm)^foo$").unwrap();
        assert_eq!(Some(2..5), re.find("\r\nfoo\r\n").map(|m| m.range()));
    }
}
