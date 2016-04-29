// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::hash_map;
use std::fmt;
use std::ops::Index;
use std::str::FromStr;
use std::sync::Arc;

use memchr::memchr;

use exec::{Exec, ExecNoSync};
use expand::expand;
use error::Error;
use re_builder::bytes::RegexBuilder;
use re_trait::{self, RegularExpression, Slot};

/// A compiled regular expression for matching arbitrary bytes.
///
/// It can be used to search, split or replace text. All searching is done with
/// an implicit `.*?` at the beginning and end of an expression. To force an
/// expression to match the whole string (or a prefix or a suffix), you must
/// use an anchor like `^` or `$` (or `\A` and `\z`).
///
/// Like the `Regex` type in the parent module, matches with this regex return
/// byte offsets into the search text. **Unlike** the parent `Regex` type,
/// these byte offsets may not correspond to UTF-8 sequence boundaries since
/// the regexes in this module can match arbitrary bytes.
#[derive(Clone)]
pub struct Regex(Exec);

impl fmt::Display for Regex {
    /// Shows the original regular expression.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Debug for Regex {
    /// Shows the original regular expression.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// A constructor for Regex from an Exec.
///
/// This is hidden because Exec isn't actually part of the public API.
#[doc(hidden)]
impl From<Exec> for Regex {
    fn from(exec: Exec) -> Regex {
        Regex(exec)
    }
}

impl FromStr for Regex {
    type Err = Error;

    /// Attempts to parse a string into a regular expression
    fn from_str(s: &str) -> Result<Regex, Error> {
        Regex::new(s)
    }
}

impl Regex {
    /// Compiles a regular expression. Once compiled, it can be used repeatedly
    /// to search, split or replace text in a string.
    ///
    /// If an invalid expression is given, then an error is returned.
    pub fn new(re: &str) -> Result<Regex, Error> {
        Regex::with_size_limit(10 * (1 << 20), re)
    }

    /// Compiles a regular expression with the given size limit.
    ///
    /// The size limit is applied to the size of the *compiled* data structure.
    /// If the data structure exceeds the size given, then an error is
    /// returned.
    pub fn with_size_limit(size: usize, re: &str) -> Result<Regex, Error> {
        RegexBuilder::new(re).size_limit(size).compile()
    }

    /// Returns true if and only if the regex matches the string given.
    ///
    /// It is recommended to use this method if all you need to do is test
    /// a match, since the underlying matching engine may be able to do less
    /// work.
    ///
    /// # Example
    ///
    /// Test if some text contains at least one word with exactly 13 ASCII word
    /// bytes:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let text = b"I categorically deny having triskaidekaphobia.";
    /// assert!(Regex::new(r"\b\w{13}\b").unwrap().is_match(text));
    /// # }
    /// ```
    pub fn is_match(&self, text: &[u8]) -> bool {
        self.is_match_at(text, 0)
    }

    /// Returns the same as is_match, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    #[doc(hidden)]
    pub fn is_match_at(&self, text: &[u8], start: usize) -> bool {
        self.shortest_match_at(text, start).is_some()
    }

    /// Returns the start and end byte range of the leftmost-first match in
    /// `text`. If no match exists, then `None` is returned.
    ///
    /// Note that this should only be used if you want to discover the position
    /// of the match. Testing the existence of a match is faster if you use
    /// `is_match`.
    ///
    /// # Example
    ///
    /// Find the start and end location of the first word with exactly 13
    /// ASCII word bytes:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let text = b"I categorically deny having triskaidekaphobia.";
    /// let pos = Regex::new(r"\b\w{13}\b").unwrap().find(text);
    /// assert_eq!(pos, Some((2, 15)));
    /// # }
    /// ```
    pub fn find(&self, text: &[u8]) -> Option<(usize, usize)> {
        self.find_at(text, 0)
    }

    /// Returns the same as find, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    #[doc(hidden)]
    pub fn find_at(
        &self,
        text: &[u8],
        start: usize,
    ) -> Option<(usize, usize)> {
        self.0.searcher().find_at(text, start)
    }

    /// Returns an iterator for each successive non-overlapping match in
    /// `text`, returning the start and end byte indices with respect to
    /// `text`.
    ///
    /// # Example
    ///
    /// Find the start and end location of every word with exactly 13 ASCII
    /// word bytes:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let text = b"Retroactively relinquishing remunerations is reprehensible.";
    /// for pos in Regex::new(r"\b\w{13}\b").unwrap().find_iter(text) {
    ///     println!("{:?}", pos);
    /// }
    /// // Output:
    /// // (0, 13)
    /// // (14, 27)
    /// // (28, 41)
    /// // (45, 58)
    /// # }
    /// ```
    pub fn find_iter<'r, 't>(&'r self, text: &'t [u8]) -> FindMatches<'r, 't> {
        FindMatches(self.0.searcher().find_iter(text))
    }

    /// Returns the capture groups corresponding to the leftmost-first
    /// match in `text`. Capture group `0` always corresponds to the entire
    /// match. If no match is found, then `None` is returned.
    ///
    /// You should only use `captures` if you need access to submatches.
    /// Otherwise, `find` is faster for discovering the location of the overall
    /// match.
    ///
    /// # Examples
    ///
    /// Say you have some text with movie names and their release years,
    /// like "'Citizen Kane' (1941)". It'd be nice if we could search for text
    /// looking like that, while also extracting the movie name and its release
    /// year separately.
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"'([^']+)'\s+\((\d{4})\)").unwrap();
    /// let text = b"Not my favorite movie: 'Citizen Kane' (1941).";
    /// let caps = re.captures(text).unwrap();
    /// assert_eq!(caps.at(1), Some(&b"Citizen Kane"[..]));
    /// assert_eq!(caps.at(2), Some(&b"1941"[..]));
    /// assert_eq!(caps.at(0), Some(&b"'Citizen Kane' (1941)"[..]));
    /// // You can also access the groups by index using the Index notation.
    /// // Note that this will panic on an invalid index.
    /// assert_eq!(&caps[1], b"Citizen Kane");
    /// assert_eq!(&caps[2], b"1941");
    /// assert_eq!(&caps[0], b"'Citizen Kane' (1941)");
    /// # }
    /// ```
    ///
    /// Note that the full match is at capture group `0`. Each subsequent
    /// capture group is indexed by the order of its opening `(`.
    ///
    /// We can make this example a bit clearer by using *named* capture groups:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"'(?P<title>[^']+)'\s+\((?P<year>\d{4})\)")
    ///                .unwrap();
    /// let text = b"Not my favorite movie: 'Citizen Kane' (1941).";
    /// let caps = re.captures(text).unwrap();
    /// assert_eq!(caps.name("title"), Some(&b"Citizen Kane"[..]));
    /// assert_eq!(caps.name("year"), Some(&b"1941"[..]));
    /// assert_eq!(caps.at(0), Some(&b"'Citizen Kane' (1941)"[..]));
    /// // You can also access the groups by name using the Index notation.
    /// // Note that this will panic on an invalid group name.
    /// assert_eq!(&caps["title"], b"Citizen Kane");
    /// assert_eq!(&caps["year"], b"1941");
    /// assert_eq!(&caps[0], b"'Citizen Kane' (1941)");
    ///
    /// # }
    /// ```
    ///
    /// Here we name the capture groups, which we can access with the `name`
    /// method or the `Index` notation with a `&str`. Note that the named
    /// capture groups are still accessible with `at` or the `Index` notation
    /// with a `usize`.
    ///
    /// The `0`th capture group is always unnamed, so it must always be
    /// accessed with `at(0)` or `[0]`.
    pub fn captures<'t>(&self, text: &'t [u8]) -> Option<Captures<'t>> {
        let mut slots = vec![None; 2 * self.captures_len()];
        self.read_captures_at(&mut slots, text, 0).map(|_| Captures {
            text: text,
            slots: slots,
            named_groups: self.0.capture_name_idx().clone(),
        })
    }

    /// Returns the same as captures, but starts the search at the given
    /// offset and populates the capture locations given.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    #[doc(hidden)]
    pub fn read_captures_at(
        &self,
        slots: &mut [Slot],
        text: &[u8],
        start: usize,
    ) -> Option<(usize, usize)> {
        self.0.searcher().read_captures_at(slots, text, start)
    }

    /// Returns an iterator over all the non-overlapping capture groups matched
    /// in `text`. This is operationally the same as `find_iter`, except it
    /// yields information about submatches.
    ///
    /// # Example
    ///
    /// We can use this to find all movie titles and their release years in
    /// some text, where the movie is formatted like "'Title' (xxxx)":
    ///
    /// ```rust
    /// # extern crate regex; use std::str; use regex::bytes::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"'(?P<title>[^']+)'\s+\((?P<year>\d{4})\)")
    ///                .unwrap();
    /// let text = b"'Citizen Kane' (1941), 'The Wizard of Oz' (1939), 'M' (1931).";
    /// for caps in re.captures_iter(text) {
    ///     let title = str::from_utf8(&caps["title"]).unwrap();
    ///     let year = str::from_utf8(&caps["year"]).unwrap();
    ///     println!("Movie: {:?}, Released: {:?}", title, year);
    /// }
    /// // Output:
    /// // Movie: Citizen Kane, Released: 1941
    /// // Movie: The Wizard of Oz, Released: 1939
    /// // Movie: M, Released: 1931
    /// # }
    /// ```
    pub fn captures_iter<'r, 't>(
        &'r self,
        text: &'t [u8],
    ) -> FindCaptures<'r, 't> {
        FindCaptures(self.0.searcher().captures_iter(text))
    }

    /// Returns an iterator of substrings of `text` delimited by a match of the
    /// regular expression. Namely, each element of the iterator corresponds to
    /// text that *isn't* matched by the regular expression.
    ///
    /// This method will *not* copy the text given.
    ///
    /// # Example
    ///
    /// To split a string delimited by arbitrary amounts of spaces or tabs:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"[ \t]+").unwrap();
    /// let fields: Vec<&[u8]> = re.split(b"a b \t  c\td    e").collect();
    /// assert_eq!(fields, vec![
    ///     &b"a"[..], &b"b"[..], &b"c"[..], &b"d"[..], &b"e"[..],
    /// ]);
    /// # }
    /// ```
    pub fn split<'r, 't>(&'r self, text: &'t [u8]) -> Splits<'r, 't> {
        Splits {
            finder: self.find_iter(text),
            last: 0,
        }
    }

    /// Returns an iterator of at most `limit` substrings of `text` delimited
    /// by a match of the regular expression. (A `limit` of `0` will return no
    /// substrings.) Namely, each element of the iterator corresponds to text
    /// that *isn't* matched by the regular expression. The remainder of the
    /// string that is not split will be the last element in the iterator.
    ///
    /// This method will *not* copy the text given.
    ///
    /// # Example
    ///
    /// Get the first two words in some text:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"\W+").unwrap();
    /// let fields: Vec<&[u8]> = re.splitn(b"Hey! How are you?", 3).collect();
    /// assert_eq!(fields, vec![&b"Hey"[..], &b"How"[..], &b"are you?"[..]]);
    /// # }
    /// ```
    pub fn splitn<'r, 't>(
        &'r self,
        text: &'t [u8],
        limit: usize,
    ) -> SplitsN<'r, 't> {
        SplitsN {
            splits: self.split(text),
            n: limit,
        }
    }

    /// Replaces the leftmost-first match with the replacement provided. The
    /// replacement can be a regular byte string (where `$N` and `$name` are
    /// expanded to match capture groups) or a function that takes the matches'
    /// `Captures` and returns the replaced byte string.
    ///
    /// If no match is found, then a copy of the byte string is returned
    /// unchanged.
    ///
    /// # Examples
    ///
    /// Note that this function is polymorphic with respect to the replacement.
    /// In typical usage, this can just be a normal byte string:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let re = Regex::new("[^01]+").unwrap();
    /// assert_eq!(re.replace(b"1078910", &b""[..]), b"1010");
    /// # }
    /// ```
    ///
    /// But anything satisfying the `Replacer` trait will work. For example, a
    /// closure of type `|&Captures| -> Vec<u8>` provides direct access to the
    /// captures corresponding to a match. This allows one to access submatches
    /// easily:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # use regex::bytes::Captures; fn main() {
    /// let re = Regex::new(r"([^,\s]+),\s+(\S+)").unwrap();
    /// let result = re.replace(b"Springsteen, Bruce", |caps: &Captures| {
    ///     let mut replacement = caps[2].to_owned();
    ///     replacement.push(b' ');
    ///     replacement.extend(&caps[1]);
    ///     replacement
    /// });
    /// assert_eq!(result, b"Bruce Springsteen");
    /// # }
    /// ```
    ///
    /// But this is a bit cumbersome to use all the time. Instead, a simple
    /// syntax is supported that expands `$name` into the corresponding capture
    /// group. Here's the last example, but using this expansion technique
    /// with named capture groups:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"(?P<last>[^,\s]+),\s+(?P<first>\S+)").unwrap();
    /// let result = re.replace(b"Springsteen, Bruce", &b"$first $last"[..]);
    /// assert_eq!(result, b"Bruce Springsteen");
    /// # }
    /// ```
    ///
    /// Note that using `$2` instead of `$first` or `$1` instead of `$last`
    /// would produce the same result. To write a literal `$` use `$$`.
    ///
    /// If `$name` isn't a valid capture group (whether the name doesn't exist
    /// or isn't a valid index), then it is replaced with the empty string.
    ///
    /// The longest possible name is used. e.g., `$1a` looks up the capture
    /// group named `1a` and not the capture group at index `1`. To exert more
    /// precise control over the name, use braces, e.g., `${1}a`.
    ///
    /// Finally, sometimes you just want to replace a literal string with no
    /// submatch expansion. This can be done by wrapping a byte string with
    /// `NoExpand`:
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// use regex::bytes::NoExpand;
    ///
    /// let re = Regex::new(r"(?P<last>[^,\s]+),\s+(\S+)").unwrap();
    /// let result = re.replace(b"Springsteen, Bruce", NoExpand(b"$2 $last"));
    /// assert_eq!(result, b"$2 $last");
    /// # }
    /// ```
    pub fn replace<R: Replacer>(&self, text: &[u8], rep: R) -> Vec<u8> {
        self.replacen(text, 1, rep)
    }

    /// Replaces all non-overlapping matches in `text` with the replacement
    /// provided. This is the same as calling `replacen` with `limit` set to
    /// `0`.
    ///
    /// See the documentation for `replace` for details on how to access
    /// submatches in the replacement text.
    pub fn replace_all<R: Replacer>(&self, text: &[u8], rep: R) -> Vec<u8> {
        self.replacen(text, 0, rep)
    }

    /// Replaces at most `limit` non-overlapping matches in `text` with the
    /// replacement provided. If `limit` is 0, then all non-overlapping matches
    /// are replaced.
    ///
    /// See the documentation for `replace` for details on how to access
    /// submatches in the replacement text.
    pub fn replacen<R: Replacer>(
        &self,
        text: &[u8],
        limit: usize,
        mut rep: R,
    ) -> Vec<u8> {
        if let Some(rep) = rep.no_expansion() {
            let mut new = Vec::with_capacity(text.len());
            let mut last_match = 0;
            for (i, (s, e)) in self.find_iter(text).enumerate() {
                if limit > 0 && i >= limit {
                    break
                }
                extend_from_slice(&mut new, &text[last_match..s]);
                extend_from_slice(&mut new, &*rep);
                last_match = e;
            }
            extend_from_slice(&mut new, &text[last_match..]);
            return new;
        }

        // The slower path, which we use if the replacement needs access to
        // capture groups.
        let mut new = Vec::with_capacity(text.len());
        let mut last_match = 0;
        for (i, cap) in self.captures_iter(text).enumerate() {
            if limit > 0 && i >= limit {
                break
            }
            // unwrap on 0 is OK because captures only reports matches
            let (s, e) = cap.pos(0).unwrap();
            extend_from_slice(&mut new, &text[last_match..s]);
            rep.replace_append(&cap, &mut new);
            last_match = e;
        }
        extend_from_slice(&mut new, &text[last_match..]);
        new
    }

    /// Returns the end location of a match in the text given.
    ///
    /// This method may have the same performance characteristics as
    /// `is_match`, except it provides an end location for a match. In
    /// particular, the location returned *may be shorter* than the proper end
    /// of the leftmost-first match.
    ///
    /// # Example
    ///
    /// Typically, `a+` would match the entire first sequence of `a` in some
    /// text, but `shortest_match` can give up as soon as it sees the first
    /// `a`.
    ///
    /// ```rust
    /// # extern crate regex; use regex::bytes::Regex;
    /// # fn main() {
    /// let text = b"aaaaa";
    /// let pos = Regex::new(r"a+").unwrap().shortest_match(text);
    /// assert_eq!(pos, Some(1));
    /// # }
    /// ```
    pub fn shortest_match(&self, text: &[u8]) -> Option<usize> {
        self.shortest_match_at(text, 0)
    }

    /// Returns the same as shortest_match, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    #[doc(hidden)]
    pub fn shortest_match_at(
        &self,
        text: &[u8],
        start: usize,
    ) -> Option<usize> {
        self.0.searcher().shortest_match_at(text, start)
    }

    /// Returns the original string of this regex.
    pub fn as_str(&self) -> &str {
        &self.0.regex_strings()[0]
    }

    /// Returns an iterator over the capture names.
    pub fn capture_names(&self) -> CaptureNames {
        CaptureNames(self.0.capture_names().iter())
    }

    /// Returns the number of captures.
    pub fn captures_len(&self) -> usize {
        self.0.capture_names().len()
    }
}

/// An iterator over all non-overlapping matches for a particular string.
///
/// The iterator yields a tuple of integers corresponding to the start and end
/// of the match. The indices are byte offsets. The iterator stops when no more
/// matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the matched byte string.
pub struct FindMatches<'r, 't>(re_trait::FindMatches<'t, ExecNoSync<'r>>);

impl<'r, 't> Iterator for FindMatches<'r, 't> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        self.0.next()
    }
}

/// An iterator that yields all non-overlapping capture groups matching a
/// particular regular expression.
///
/// The iterator stops when no more matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the matched byte string.
pub struct FindCaptures<'r, 't>(re_trait::FindCaptures<'t, ExecNoSync<'r>>);

impl<'r, 't> Iterator for FindCaptures<'r, 't> {
    type Item = Captures<'t>;

    fn next(&mut self) -> Option<Captures<'t>> {
        self.0.next().map(|slots| Captures {
            text: self.0.text(),
            slots: slots,
            named_groups: self.0.regex().capture_name_idx().clone(),
        })
    }
}

/// Yields all substrings delimited by a regular expression match.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the byte string being split.
pub struct Splits<'r, 't> {
    finder: FindMatches<'r, 't>,
    last: usize,
}

impl<'r, 't> Iterator for Splits<'r, 't> {
    type Item = &'t [u8];

    fn next(&mut self) -> Option<&'t [u8]> {
        let text = self.finder.0.text();
        match self.finder.next() {
            None => {
                if self.last >= text.len() {
                    None
                } else {
                    let s = &text[self.last..];
                    self.last = text.len();
                    Some(s)
                }
            }
            Some((s, e)) => {
                let matched = &text[self.last..s];
                self.last = e;
                Some(matched)
            }
        }
    }
}

/// Yields at most `N` substrings delimited by a regular expression match.
///
/// The last substring will be whatever remains after splitting.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the byte string being split.
pub struct SplitsN<'r, 't> {
    splits: Splits<'r, 't>,
    n: usize,
}

impl<'r, 't> Iterator for SplitsN<'r, 't> {
    type Item = &'t [u8];

    fn next(&mut self) -> Option<&'t [u8]> {
        if self.n == 0 {
            return None
        }
        self.n -= 1;
        if self.n == 0 {
            let text = self.splits.finder.0.text();
            Some(&text[self.splits.last..])
        } else {
            self.splits.next()
        }
    }
}

/// An iterator over the names of all possible captures.
///
/// `None` indicates an unnamed capture; the first element (capture 0, the
/// whole matched region) is always unnamed.
///
/// `'r` is the lifetime of the compiled regular expression.
pub struct CaptureNames<'r>(::std::slice::Iter<'r, Option<String>>);

impl<'r> Iterator for CaptureNames<'r> {
    type Item = Option<&'r str>;

    fn next(&mut self) -> Option<Option<&'r str>> {
        self.0.next().as_ref()
            .map(|slot| slot.as_ref().map(|name| name.as_ref()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

/// Captures represents a group of captured byte strings for a single match.
///
/// The 0th capture always corresponds to the entire match. Each subsequent
/// index corresponds to the next capture group in the regex. If a capture
/// group is named, then the matched byte string is *also* available via the
/// `name` method. (Note that the 0th capture is always unnamed and so must be
/// accessed with the `at` method.)
///
/// Positions returned from a capture group are always byte indices.
///
/// `'t` is the lifetime of the matched text.
pub struct Captures<'t> {
    text: &'t [u8],
    slots: Vec<Option<usize>>,
    named_groups: Arc<HashMap<String, usize>>,
}

impl<'t> Captures<'t> {
    /// Returns the start and end positions of the Nth capture group. Returns
    /// `None` if `i` is not a valid capture group or if the capture group did
    /// not match anything. The positions returned are *always* byte indices
    /// with respect to the original byte string matched.
    pub fn pos(&self, i: usize) -> Option<(usize, usize)> {
        let (s, e) = (i * 2, i * 2 + 1);
        match (self.slots.get(s), self.slots.get(e)) {
            (Some(&Some(s)), Some(&Some(e))) => Some((s, e)),
            _ => None,
        }
    }

    /// Returns the matched string for the capture group `i`.  If `i` isn't
    /// a valid capture group or didn't match anything, then `None` is
    /// returned.
    pub fn at(&self, i: usize) -> Option<&'t [u8]> {
        match self.pos(i) {
            None => None,
            Some((s, e)) => Some(&self.text[s..e])
        }
    }

    /// Returns the matched string for the capture group named `name`.  If
    /// `name` isn't a valid capture group or didn't match anything, then
    /// `None` is returned.
    pub fn name(&self, name: &str) -> Option<&'t [u8]> {
        self.named_groups.get(name).and_then(|&i| self.at(i))
    }

    /// Creates an iterator of all the capture groups in order of appearance
    /// in the regular expression.
    pub fn iter<'a>(&'a self) -> SubCaptures<'a, 't> {
        SubCaptures { idx: 0, caps: self }
    }

    /// Creates an iterator of all the capture group positions in order of
    /// appearance in the regular expression. Positions are byte indices
    /// in terms of the original string matched.
    pub fn iter_pos(&'t self) -> SubCapturesPos<'t> {
        SubCapturesPos { idx: 0, slots: &self.slots }
    }

    /// Creates an iterator of all named groups as an tuple with the group
    /// name and the value. The iterator returns these values in arbitrary
    /// order.
    pub fn iter_named<'a>(&'a self) -> SubCapturesNamed<'a, 't> {
        SubCapturesNamed {
            caps: self,
            names: self.named_groups.iter()
        }
    }

    /// Expands all instances of `$name` in `text` to the corresponding capture
    /// group `name`, and writes them to the `dst` buffer given.
    ///
    /// `name` may be an integer corresponding to the index of the
    /// capture group (counted by order of opening parenthesis where `0` is the
    /// entire match) or it can be a name (consisting of letters, digits or
    /// underscores) corresponding to a named capture group.
    ///
    /// If `name` isn't a valid capture group (whether the name doesn't exist
    /// or isn't a valid index), then it is replaced with the empty string.
    ///
    /// The longest possible name is used. e.g., `$1a` looks up the capture
    /// group named `1a` and not the capture group at index `1`. To exert more
    /// precise control over the name, use braces, e.g., `${1}a`.
    ///
    /// To write a literal `$` use `$$`.
    pub fn expand(&self, replacement: &[u8], dst: &mut Vec<u8>) {
        expand(self, replacement, dst)
    }

    /// Returns the number of captured groups.
    #[inline]
    pub fn len(&self) -> usize {
        self.slots.len() / 2
    }

    /// Returns true if and only if there are no captured groups.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'t> fmt::Debug for Captures<'t> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Captures").field(&CapturesDebug(self)).finish()
    }
}

struct CapturesDebug<'c, 't: 'c>(&'c Captures<'t>);

impl<'c, 't> fmt::Debug for CapturesDebug<'c, 't> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn escape_bytes(bytes: &[u8]) -> String {
            let mut s = String::new();
            for &b in bytes {
                s.push_str(&escape_byte(b));
            }
            s
        }

        fn escape_byte(byte: u8) -> String {
            use std::ascii::escape_default;

            let escaped: Vec<u8> = escape_default(byte).collect();
            String::from_utf8_lossy(&escaped).into_owned()
        }

        // We'd like to show something nice here, even if it means an
        // allocation to build a reverse index.
        let slot_to_name: HashMap<&usize, &String> =
            self.0.named_groups.iter().map(|(a, b)| (b, a)).collect();
        let mut map = f.debug_map();
        for (slot, m) in self.0.iter_pos().enumerate() {
            let m = m.map(|(s, e)| escape_bytes(&self.0.text[s..e]));
            if let Some(ref name) = slot_to_name.get(&slot) {
                map.entry(&name, &m);
            } else {
                map.entry(&slot, &m);
            }
        }
        map.finish()
    }
}

/// Get a group by index.
///
/// `'t` is the lifetime of the matched text.
///
/// The text can't outlive the `Captures` object if this method is
/// used, because of how `Index` is defined (normally `a[i]` is part
/// of `a` and can't outlive it); to do that, use `at()` instead.
///
/// # Panics
///
/// If there is no group at the given index.
impl<'t> Index<usize> for Captures<'t> {
    type Output = [u8];

    fn index(&self, i: usize) -> &[u8] {
        self.at(i).unwrap_or_else(|| panic!("no group at index '{}'", i))
    }
}

/// Get a group by name.
///
/// `'t` is the lifetime of the matched text and `'i` is the lifetime
/// of the group name (the index).
///
/// The text can't outlive the `Captures` object if this method is
/// used, because of how `Index` is defined (normally `a[i]` is part
/// of `a` and can't outlive it); to do that, use `name` instead.
///
/// # Panics
///
/// If there is no group named by the given value.
impl<'t, 'i> Index<&'i str> for Captures<'t> {
    type Output = [u8];

    fn index<'a>(&'a self, name: &'i str) -> &'a [u8] {
        self.name(name).unwrap_or_else(|| panic!("no group named '{}'", name))
    }
}

/// An iterator over capture groups for a particular match of a regular
/// expression.
///
/// `'c` is the lifetime of the captures and `'t` is the lifetime of the
/// matched text.
pub struct SubCaptures<'c, 't: 'c> {
    idx: usize,
    caps: &'c Captures<'t>,
}

impl<'c, 't> Iterator for SubCaptures<'c, 't> {
    type Item = Option<&'t [u8]>;

    fn next(&mut self) -> Option<Option<&'t [u8]>> {
        if self.idx < self.caps.len() {
            self.idx += 1;
            Some(self.caps.at(self.idx - 1))
        } else {
            None
        }
    }
}

/// An iterator over capture group positions for a particular match of a
/// regular expression.
///
/// Positions are byte indices in terms of the original byte string matched.
///
/// `'c` is the lifetime of the captures.
pub struct SubCapturesPos<'c> {
    idx: usize,
    slots: &'c [Option<usize>]
}

impl<'c> Iterator for SubCapturesPos<'c> {
    type Item = Option<(usize, usize)>;

    fn next(&mut self) -> Option<Option<(usize, usize)>> {
        if self.idx >= self.slots.len() {
            return None
        }
        let r = match (self.slots[self.idx], self.slots[self.idx + 1]) {
            (Some(s), Some(e)) => Some((s, e)),
            _ => None,
        };
        self.idx += 2;
        Some(r)
    }
}

/// An Iterator over named capture groups as a tuple with the group name and
/// the value.
///
/// `'c` is the lifetime of the captures and `'t` is the lifetime of the
/// matched text.
pub struct SubCapturesNamed<'c, 't: 'c> {
    caps: &'c Captures<'t>,
    names: hash_map::Iter<'c, String, usize>,
}

impl<'c, 't> Iterator for SubCapturesNamed<'c, 't> {
    type Item = (&'c str, Option<&'t [u8]>);

    fn next(&mut self) -> Option<(&'c str, Option<&'t [u8]>)> {
        self.names.next().map(|(name, &pos)| (&**name, self.caps.at(pos)))
    }
}

/// Replacer describes types that can be used to replace matches in a byte
/// string.
///
/// In general, users of this crate shouldn't need to implement this trait,
/// since implementations are already provided for `&[u8]` and
/// `FnMut(&Captures) -> Vec<u8>`, which covers most use cases.
pub trait Replacer {
    /// Appends text to `dst` to replace the current match.
    ///
    /// The current match is represented by `caps`, which is guaranteed to
    /// have a match at capture group `0`.
    ///
    /// For example, a no-op replacement would be
    /// `dst.extend(caps.at(0).unwrap())`.
    fn replace_append(&mut self, caps: &Captures, dst: &mut Vec<u8>);

    /// Return a fixed unchanging replacement byte string.
    ///
    /// When doing replacements, if access to `Captures` is not needed (e.g.,
    /// the replacement byte string does not need `$` expansion), then it can
    /// be beneficial to avoid finding sub-captures.
    ///
    /// In general, this is called once for every call to `replacen`.
    fn no_expansion<'r>(&'r mut self) -> Option<Cow<'r, [u8]>> {
        None
    }
}

impl<'a> Replacer for &'a [u8] {
    fn replace_append(&mut self, caps: &Captures, dst: &mut Vec<u8>) {
        caps.expand(*self, dst);
    }

    fn no_expansion<'r>(&'r mut self) -> Option<Cow<'r, [u8]>> {
        match memchr(b'$', *self) {
            Some(_) => None,
            None => Some(Cow::Borrowed(*self)),
        }
    }
}

impl<F> Replacer for F where F: FnMut(&Captures) -> Vec<u8> {
    fn replace_append(&mut self, caps: &Captures, dst: &mut Vec<u8>) {
        extend_from_slice(dst, &(*self)(caps));
    }
}

/// NoExpand indicates literal byte string replacement.
///
/// It can be used with `replace` and `replace_all` to do a literal byte string
/// replacement without expanding `$name` to their corresponding capture
/// groups. This can be both convenient (to avoid escaping `$`, for example)
/// and performant (since capture groups don't need to be found).
///
/// `'t` is the lifetime of the literal text.
pub struct NoExpand<'r>(pub &'r [u8]);

impl<'a> Replacer for NoExpand<'a> {
    fn replace_append(&mut self, _: &Captures, dst: &mut Vec<u8>) {
        extend_from_slice(dst, self.0);
    }

    fn no_expansion<'r>(&'r mut self) -> Option<Cow<'r, [u8]>> {
        Some(Cow::Borrowed(self.0))
    }
}

/// This hopefully has the same performance characteristics as
/// Vec::extend_from_slice (which was introduced in Rust 1.6), but works on
/// Rust 1.3.
///
/// N.B. Remove this once we do a semver bump. At that point, we'll bump
/// required Rust version to at least 1.6.
fn extend_from_slice(dst: &mut Vec<u8>, src: &[u8]) {
    dst.reserve(src.len());
    let dst_len = dst.len();
    unsafe { dst.set_len(dst_len + src.len()); }
    let mut dst = &mut dst[dst_len..dst_len + src.len()];
    for i in 0..src.len() {
        dst[i] = src[i];
    }
}
