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
use std::fmt;
use std::ops::Index;
#[cfg(feature = "pattern")]
use std::str::pattern::{Pattern, Searcher, SearchStep};
use std::str::FromStr;
use std::sync::Arc;

use syntax;

use exec::{CaptureSlots, Exec, ExecBuilder, Search};
use error::Error;

/// Escapes all regular expression meta characters in `text`.
///
/// The string returned may be safely used as a literal in a regular
/// expression.
pub fn quote(text: &str) -> String {
    syntax::quote(text)
}

/// Tests if the given regular expression matches somewhere in the text given.
///
/// If there was a problem compiling the regular expression, an error is
/// returned.
///
/// To find submatches, split or replace text, you'll need to compile an
/// expression first.
pub fn is_match(regex: &str, text: &str) -> Result<bool, Error> {
    Regex::new(regex).map(|r| r.is_match(text))
}

/// A compiled regular expression for matching Unicode strings.
///
/// It is represented as either a sequence of bytecode instructions (dynamic)
/// or as a specialized Rust function (native). It can be used to search, split
/// or replace text. All searching is done with an implicit `.*?` at the
/// beginning and end of an expression. To force an expression to match the
/// whole string (or a prefix or a suffix), you must use an anchor like `^` or
/// `$` (or `\A` and `\z`).
///
/// While this crate will handle Unicode strings (whether in the regular
/// expression or in the search text), all positions returned are **byte
/// indices**. Every byte index is guaranteed to be at a Unicode code point
/// boundary.
///
/// The lifetimes `'r` and `'t` in this crate correspond to the lifetime of a
/// compiled regular expression and text to search, respectively.
///
/// The only methods that allocate new strings are the string replacement
/// methods. All other methods (searching and splitting) return borrowed
/// pointers into the string given.
///
/// # Examples
///
/// Find the location of a US phone number:
///
/// ```rust
/// # use regex::Regex;
/// let re = Regex::new("[0-9]{3}-[0-9]{3}-[0-9]{4}").unwrap();
/// assert_eq!(re.find("phone: 111-222-3333"), Some((7, 19)));
/// ```
///
/// # Using the `std::str::StrExt` methods with `Regex`
///
/// > **Note**: This section requires that this crate is currently compiled with
/// >           the `pattern` Cargo feature enabled.
///
/// Since `Regex` implements `Pattern`, you can use regexes with methods
/// defined on `std::str::StrExt`. For example, `is_match`, `find`, `find_iter`
/// and `split` can be replaced with `StrExt::contains`, `StrExt::find`,
/// `StrExt::match_indices` and `StrExt::split`.
///
/// Here are some examples:
///
/// ```rust,ignore
/// # use regex::Regex;
/// let re = Regex::new(r"\d+").unwrap();
/// let haystack = "a111b222c";
///
/// assert!(haystack.contains(&re));
/// assert_eq!(haystack.find(&re), Some(1));
/// assert_eq!(haystack.match_indices(&re).collect::<Vec<_>>(),
///            vec![(1, 4), (5, 8)]);
/// assert_eq!(haystack.split(&re).collect::<Vec<_>>(), vec!["a", "b", "c"]);
/// ```
#[derive(Clone)]
pub struct Regex(#[doc(hidden)] pub _Regex);

#[derive(Clone)]
#[doc(hidden)]
pub enum _Regex {
    // The representation of `Regex` is exported to support the `regex!`
    // syntax extension. Do not rely on it.
    //
    // See the comments for the `program` module in `lib.rs` for a more
    // detailed explanation for what `regex!` requires.
    #[doc(hidden)]
    Dynamic(Exec),
    #[doc(hidden)]
    Native(ExNative),
}

#[doc(hidden)]
pub struct ExNative {
    #[doc(hidden)]
    pub original: &'static str,
    #[doc(hidden)]
    pub names: &'static &'static [Option<&'static str>],
    #[doc(hidden)]
    pub groups: &'static &'static [(&'static str, usize)],
    #[doc(hidden)]
    pub prog: fn(CaptureSlots, &str, usize) -> bool,
}

impl Copy for ExNative {}

impl Clone for ExNative {
    fn clone(&self) -> ExNative {
        *self
    }
}

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

#[doc(hidden)]
impl From<Exec> for Regex {
    fn from(exec: Exec) -> Regex {
        Regex(_Regex::Dynamic(exec))
    }
}

/// Equality comparison is based on the original string. It is possible that
/// different regular expressions have the same matching behavior, but are
/// still compared unequal. For example, `\d+` and `\d\d*` match the same set
/// of strings, but are not considered equal.
impl PartialEq for Regex {
    fn eq(&self, other: &Regex) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for Regex {}

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
        ExecBuilder::new(re).size_limit(size).build().map(Regex::from)
    }

    /// Returns true if and only if the regex matches the string given.
    ///
    /// It is recommended to use this method if all you need to do is test
    /// a match, since the underlying matching engine may be able to do less
    /// work.
    ///
    /// # Example
    ///
    /// Test if some text contains at least one word with exactly 13
    /// Unicode word characters:
    ///
    /// ```rust
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let text = "I categorically deny having triskaidekaphobia.";
    /// assert!(Regex::new(r"\b\w{13}\b").unwrap().is_match(text));
    /// # }
    /// ```
    pub fn is_match(&self, text: &str) -> bool {
        self.exec(&mut [], text, 0)
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
    /// Unicode word characters:
    ///
    /// ```rust
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let text = "I categorically deny having triskaidekaphobia.";
    /// let pos = Regex::new(r"\b\w{13}\b").unwrap().find(text);
    /// assert_eq!(pos, Some((2, 15)));
    /// # }
    /// ```
    pub fn find(&self, text: &str) -> Option<(usize, usize)> {
        let mut caps = [None, None];
        if !self.exec(&mut caps, text, 0) {
            None
        } else {
            Some((caps[0].unwrap(), caps[1].unwrap()))
        }
    }

    /// Returns an iterator for each successive non-overlapping match in
    /// `text`, returning the start and end byte indices with respect to
    /// `text`.
    ///
    /// # Example
    ///
    /// Find the start and end location of every word with exactly 13 Unicode
    /// word characters:
    ///
    /// ```rust
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let text = "Retroactively relinquishing remunerations is reprehensible.";
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
    pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> FindMatches<'r, 't> {
        FindMatches {
            re: self,
            text: text,
            last_end: 0,
            last_match: None,
        }
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
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"'([^']+)'\s+\((\d{4})\)").unwrap();
    /// let text = "Not my favorite movie: 'Citizen Kane' (1941).";
    /// let caps = re.captures(text).unwrap();
    /// assert_eq!(caps.at(1), Some("Citizen Kane"));
    /// assert_eq!(caps.at(2), Some("1941"));
    /// assert_eq!(caps.at(0), Some("'Citizen Kane' (1941)"));
    /// // You can also access the groups by index using the Index notation.
    /// // Note that this will panic on an invalid index.
    /// assert_eq!(&caps[1], "Citizen Kane");
    /// assert_eq!(&caps[2], "1941");
    /// assert_eq!(&caps[0], "'Citizen Kane' (1941)");
    /// # }
    /// ```
    ///
    /// Note that the full match is at capture group `0`. Each subsequent
    /// capture group is indexed by the order of its opening `(`.
    ///
    /// We can make this example a bit clearer by using *named* capture groups:
    ///
    /// ```rust
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"'(?P<title>[^']+)'\s+\((?P<year>\d{4})\)")
    ///                .unwrap();
    /// let text = "Not my favorite movie: 'Citizen Kane' (1941).";
    /// let caps = re.captures(text).unwrap();
    /// assert_eq!(caps.name("title"), Some("Citizen Kane"));
    /// assert_eq!(caps.name("year"), Some("1941"));
    /// assert_eq!(caps.at(0), Some("'Citizen Kane' (1941)"));
    /// // You can also access the groups by name using the Index notation.
    /// // Note that this will panic on an invalid group name.
    /// assert_eq!(&caps["title"], "Citizen Kane");
    /// assert_eq!(&caps["year"], "1941");
    /// assert_eq!(&caps[0], "'Citizen Kane' (1941)");
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
    pub fn captures<'t>(&self, text: &'t str) -> Option<Captures<'t>> {
        let mut caps = self.alloc_captures();
        if !self.exec(&mut caps, text, 0) {
            None
        } else {
            Some(Captures {
                text: text,
                caps: caps,
                named_groups: NamedGroups::from_regex(self)
            })
        }
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
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"'(?P<title>[^']+)'\s+\((?P<year>\d{4})\)")
    ///                .unwrap();
    /// let text = "'Citizen Kane' (1941), 'The Wizard of Oz' (1939), 'M' (1931).";
    /// for caps in re.captures_iter(text) {
    ///     println!("Movie: {:?}, Released: {:?}",
    ///              caps.name("title"), caps.name("year"));
    /// }
    /// // Output:
    /// // Movie: Citizen Kane, Released: 1941
    /// // Movie: The Wizard of Oz, Released: 1939
    /// // Movie: M, Released: 1931
    /// # }
    /// ```
    pub fn captures_iter<'r, 't>(
        &'r self,
        text: &'t str,
    ) -> FindCaptures<'r, 't> {
        FindCaptures {
            re: self,
            text: text,
            last_end: 0,
            last_match: None,
        }
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
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"[ \t]+").unwrap();
    /// let fields: Vec<&str> = re.split("a b \t  c\td    e").collect();
    /// assert_eq!(fields, vec!["a", "b", "c", "d", "e"]);
    /// # }
    /// ```
    pub fn split<'r, 't>(&'r self, text: &'t str) -> RegexSplits<'r, 't> {
        RegexSplits {
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
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"\W+").unwrap();
    /// let fields: Vec<&str> = re.splitn("Hey! How are you?", 3).collect();
    /// assert_eq!(fields, vec!("Hey", "How", "are you?"));
    /// # }
    /// ```
    pub fn splitn<'r, 't>(&'r self, text: &'t str, limit: usize)
                         -> RegexSplitsN<'r, 't> {
        RegexSplitsN {
            splits: self.split(text),
            n: limit,
        }
    }

    /// Replaces the leftmost-first match with the replacement provided.
    /// The replacement can be a regular string (where `$N` and `$name` are
    /// expanded to match capture groups) or a function that takes the matches'
    /// `Captures` and returns the replaced string.
    ///
    /// If no match is found, then a copy of the string is returned unchanged.
    ///
    /// # Examples
    ///
    /// Note that this function is polymorphic with respect to the replacement.
    /// In typical usage, this can just be a normal string:
    ///
    /// ```rust
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let re = Regex::new("[^01]+").unwrap();
    /// assert_eq!(re.replace("1078910", ""), "1010");
    /// # }
    /// ```
    ///
    /// But anything satisfying the `Replacer` trait will work. For example,
    /// a closure of type `|&Captures| -> String` provides direct access to the
    /// captures corresponding to a match. This allows one to access
    /// submatches easily:
    ///
    /// ```rust
    /// # extern crate regex; use regex::Regex;
    /// # use regex::Captures; fn main() {
    /// let re = Regex::new(r"([^,\s]+),\s+(\S+)").unwrap();
    /// let result = re.replace("Springsteen, Bruce", |caps: &Captures| {
    ///     format!("{} {}", caps.at(2).unwrap_or(""), caps.at(1).unwrap_or(""))
    /// });
    /// assert_eq!(result, "Bruce Springsteen");
    /// # }
    /// ```
    ///
    /// But this is a bit cumbersome to use all the time. Instead, a simple
    /// syntax is supported that expands `$name` into the corresponding capture
    /// group. Here's the last example, but using this expansion technique
    /// with named capture groups:
    ///
    /// ```rust
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// let re = Regex::new(r"(?P<last>[^,\s]+),\s+(?P<first>\S+)").unwrap();
    /// let result = re.replace("Springsteen, Bruce", "$first $last");
    /// assert_eq!(result, "Bruce Springsteen");
    /// # }
    /// ```
    ///
    /// Note that using `$2` instead of `$first` or `$1` instead of `$last`
    /// would produce the same result. To write a literal `$` use `$$`.
    ///
    /// Finally, sometimes you just want to replace a literal string with no
    /// submatch expansion. This can be done by wrapping a string with
    /// `NoExpand`:
    ///
    /// ```rust
    /// # extern crate regex; use regex::Regex;
    /// # fn main() {
    /// use regex::NoExpand;
    ///
    /// let re = Regex::new(r"(?P<last>[^,\s]+),\s+(\S+)").unwrap();
    /// let result = re.replace("Springsteen, Bruce", NoExpand("$2 $last"));
    /// assert_eq!(result, "$2 $last");
    /// # }
    /// ```
    pub fn replace<R: Replacer>(&self, text: &str, rep: R) -> String {
        self.replacen(text, 1, rep)
    }

    /// Replaces all non-overlapping matches in `text` with the replacement
    /// provided. This is the same as calling `replacen` with `limit` set to
    /// `0`.
    ///
    /// See the documentation for `replace` for details on how to access
    /// submatches in the replacement string.
    pub fn replace_all<R: Replacer>(&self, text: &str, rep: R) -> String {
        self.replacen(text, 0, rep)
    }

    /// Replaces at most `limit` non-overlapping matches in `text` with the
    /// replacement provided. If `limit` is 0, then all non-overlapping matches
    /// are replaced.
    ///
    /// See the documentation for `replace` for details on how to access
    /// submatches in the replacement string.
    pub fn replacen<R: Replacer>(
        &self,
        text: &str,
        limit: usize,
        mut rep: R,
    ) -> String {

        // If we know that the replacement doesn't have any capture expansions,
        // then we can fast path. The fast path can make a tremendous
        // difference:
        //
        //   1) We use `find_iter` instead of `captures_iter`. Not asking for
        //      captures generally makes the regex engines faster.
        //   2) We don't need to look up all of the capture groups and do
        //      replacements inside the replacement string. We just push it
        //      at each match and be done with it.
        if let Some(rep) = rep.no_expand() {
            let mut new = String::with_capacity(text.len());
            let mut last_match = 0;
            for (i, (s, e)) in self.find_iter(text).enumerate() {
                if limit > 0 && i >= limit {
                    break
                }
                new.push_str(&text[last_match..s]);
                new.push_str(&rep);
                last_match = e;
            }
            new.push_str(&text[last_match..]);
            return new;
        }

        // The slower path, which we use if the replacement needs access to
        // capture groups.
        let mut new = String::with_capacity(text.len());
        let mut last_match = 0;
        for (i, cap) in self.captures_iter(text).enumerate() {
            if limit > 0 && i >= limit {
                break
            }
            // unwrap on 0 is OK because captures only reports matches
            let (s, e) = cap.pos(0).unwrap();
            new.push_str(&text[last_match..s]);
            new.push_str(&rep.reg_replace(&cap));
            last_match = e;
        }
        new.push_str(&text[last_match..]);
        new
    }

    /// Returns the original string of this regex.
    pub fn as_str(&self) -> &str {
        match self.0 {
            _Regex::Dynamic(ref exec) => &exec.regex_strings()[0],
            _Regex::Native(ExNative { ref original, .. }) => original,
        }
    }

    /// Returns an iterator over the capture names.
    pub fn capture_names(&self) -> CaptureNames {
        CaptureNames(match self.0 {
            _Regex::Native(ref n) => _CaptureNames::Native(n.names.iter()),
            _Regex::Dynamic(ref d) => {
                _CaptureNames::Dynamic(d.captures().iter())
            }
        })
    }

    /// Returns the number of captures.
    pub fn captures_len(&self) -> usize {
        match self.0 {
            _Regex::Native(ref n) => n.names.len(),
            _Regex::Dynamic(ref d) => d.captures().len()
        }
    }

    fn alloc_captures(&self) -> Vec<Option<usize>> {
        match self.0 {
            _Regex::Native(ref n) => vec![None; 2 * n.names.len()],
            _Regex::Dynamic(ref d) => vec![None; 2 * d.captures().len()],
        }
    }

    fn exec(&self, caps: CaptureSlots, text: &str, start: usize) -> bool {
        match self.0 {
            _Regex::Native(ExNative { ref prog, .. }) => {
                (*prog)(caps, text, start)
            }
            _Regex::Dynamic(ref exec) => {
                let mut _matches = [false];
                let mut search = Search::new(caps, &mut _matches);
                exec.exec(&mut search, text.as_bytes(), start)
            }
        }
    }
}

/// An iterator over the names of all possible captures.
///
/// `None` indicates an unnamed capture; the first element (capture 0, the
/// whole matched region) is always unnamed.
///
/// `'r` is the lifetime of the compiled regular expression.
pub struct CaptureNames<'r>(_CaptureNames<'r>);

enum _CaptureNames<'r> {
    #[doc(hidden)]
    Native(::std::slice::Iter<'r, Option<&'static str>>),
    #[doc(hidden)]
    Dynamic(::std::slice::Iter<'r, Option<String>>)
}

impl<'r> Iterator for CaptureNames<'r> {
    type Item = Option<&'r str>;

    fn next(&mut self) -> Option<Option<&'r str>> {
        match self.0 {
            _CaptureNames::Native(ref mut i) => i.next().cloned(),
            _CaptureNames::Dynamic(ref mut i) => {
                i.next().as_ref().map(|o| o.as_ref().map(|s| s.as_ref()))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.0 {
            _CaptureNames::Native(ref i)  => i.size_hint(),
            _CaptureNames::Dynamic(ref i) => i.size_hint(),
        }
    }
}

/// NoExpand indicates literal string replacement.
///
/// It can be used with `replace` and `replace_all` to do a literal
/// string replacement without expanding `$name` to their corresponding
/// capture groups.
///
/// `'t` is the lifetime of the literal text.
pub struct NoExpand<'t>(pub &'t str);

/// Replacer describes types that can be used to replace matches in a string.
pub trait Replacer {
    /// Returns a possibly owned string that is used to replace the match
    /// corresponding to the `caps` capture group.
    ///
    /// The `'a` lifetime refers to the lifetime of a borrowed string when
    /// a new owned string isn't needed (e.g., for `NoExpand`).
    fn reg_replace(&mut self, caps: &Captures) -> Cow<str>;

    /// Returns a possibly owned string that never needs expansion.
    fn no_expand(&mut self) -> Option<Cow<str>> { None }
}

impl<'t> Replacer for NoExpand<'t> {
    fn reg_replace(&mut self, _: &Captures) -> Cow<str> {
        self.0.into()
    }

    fn no_expand(&mut self) -> Option<Cow<str>> {
        Some(self.0.into())
    }
}

impl<'t> Replacer for &'t str {
    fn reg_replace<'a>(&'a mut self, caps: &Captures) -> Cow<'a, str> {
        caps.expand(*self).into()
    }

    fn no_expand(&mut self) -> Option<Cow<str>> {
        // if there is a $ there may be an expansion
        match self.find('$') {
            Some(_) => None,
            None => Some((*self).into()),
        }
    }
}

impl<F> Replacer for F where F: FnMut(&Captures) -> String {
    fn reg_replace<'a>(&'a mut self, caps: &Captures) -> Cow<'a, str> {
        (*self)(caps).into()
    }
}

/// Yields all substrings delimited by a regular expression match.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the string being split.
pub struct RegexSplits<'r, 't> {
    finder: FindMatches<'r, 't>,
    last: usize,
}

impl<'r, 't> Iterator for RegexSplits<'r, 't> {
    type Item = &'t str;

    fn next(&mut self) -> Option<&'t str> {
        let text = self.finder.text;
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
/// lifetime of the string being split.
pub struct RegexSplitsN<'r, 't> {
    splits: RegexSplits<'r, 't>,
    n: usize,
}

impl<'r, 't> Iterator for RegexSplitsN<'r, 't> {
    type Item = &'t str;

    fn next(&mut self) -> Option<&'t str> {
        if self.n == 0 {
            return None
        }
        self.n -= 1;
        if self.n == 0 {
            let text = self.splits.finder.text;
            Some(&text[self.splits.last..])
        } else {
            self.splits.next()
        }
    }
}

enum NamedGroups {
    Native(&'static [(&'static str, usize)]),
    Dynamic(Arc<HashMap<String, usize>>),
}

impl NamedGroups {
    fn from_regex(regex: &Regex) -> NamedGroups {
        match regex.0 {
            _Regex::Native(ExNative { ref groups, .. }) => {
                NamedGroups::Native(groups)
            }
            _Regex::Dynamic(ref exec) => {
                NamedGroups::Dynamic(exec.capture_name_idx().clone())
            }
        }
    }

    fn pos(&self, name: &str) -> Option<usize> {
        match *self {
            NamedGroups::Native(groups) => {
                groups.binary_search_by(|&(n, _)| n.cmp(name))
                      .ok().map(|i| groups[i].1)
            },
            NamedGroups::Dynamic(ref groups) => {
                groups.get(name).map(|i| *i)
            },
        }
    }

    fn iter<'n>(&'n self) -> NamedGroupsIter<'n> {
        match *self {
            NamedGroups::Native(g) => NamedGroupsIter::Native(g.iter()),
            NamedGroups::Dynamic(ref g) => NamedGroupsIter::Dynamic(g.iter()),
        }
    }
}

enum NamedGroupsIter<'n> {
    Native(::std::slice::Iter<'static, (&'static str, usize)>),
    Dynamic(::std::collections::hash_map::Iter<'n, String, usize>),
}

impl<'n> Iterator for NamedGroupsIter<'n> {
    type Item = (&'n str, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            NamedGroupsIter::Native(ref mut it) => it.next().map(|&v| v),
            NamedGroupsIter::Dynamic(ref mut it) => {
                it.next().map(|(s, i)| (s.as_ref(), *i))
            }
        }
    }
}

/// Captures represents a group of captured strings for a single match.
///
/// The 0th capture always corresponds to the entire match. Each subsequent
/// index corresponds to the next capture group in the regex. If a capture
/// group is named, then the matched string is *also* available via the `name`
/// method. (Note that the 0th capture is always unnamed and so must be
/// accessed with the `at` method.)
///
/// Positions returned from a capture group are always byte indices.
///
/// `'t` is the lifetime of the matched text.
pub struct Captures<'t> {
    text: &'t str,
    caps: Vec<Option<usize>>,
    named_groups: NamedGroups,
}

impl<'t> Captures<'t> {
    /// Returns the start and end positions of the Nth capture group. Returns
    /// `None` if `i` is not a valid capture group or if the capture group did
    /// not match anything. The positions returned are *always* byte indices
    /// with respect to the original string matched.
    pub fn pos(&self, i: usize) -> Option<(usize, usize)> {
        let (s, e) = (i * 2, i * 2 + 1);
        match (self.caps.get(s), self.caps.get(e)) {
            (Some(&Some(s)), Some(&Some(e))) => Some((s, e)),
            _ => None,
        }
    }

    /// Returns the matched string for the capture group `i`.  If `i` isn't
    /// a valid capture group or didn't match anything, then `None` is
    /// returned.
    pub fn at(&self, i: usize) -> Option<&'t str> {
        match self.pos(i) {
            None => None,
            Some((s, e)) => Some(&self.text[s..e])
        }
    }

    /// Returns the matched string for the capture group named `name`.  If
    /// `name` isn't a valid capture group or didn't match anything, then
    /// `None` is returned.
    pub fn name(&self, name: &str) -> Option<&'t str> {
        self.named_groups.pos(name).and_then(|i| self.at(i))
    }

    /// Creates an iterator of all the capture groups in order of appearance
    /// in the regular expression.
    pub fn iter(&'t self) -> SubCaptures<'t> {
        SubCaptures { idx: 0, caps: self, }
    }

    /// Creates an iterator of all the capture group positions in order of
    /// appearance in the regular expression. Positions are byte indices
    /// in terms of the original string matched.
    pub fn iter_pos(&'t self) -> SubCapturesPos<'t> {
        SubCapturesPos { idx: 0, caps: &self.caps }
    }

    /// Creates an iterator of all named groups as an tuple with the group
    /// name and the value. The iterator returns these values in arbitrary
    /// order.
    pub fn iter_named(&'t self) -> SubCapturesNamed<'t> {
        SubCapturesNamed {
            caps: self,
            names: self.named_groups.iter()
        }
    }

    /// Expands all instances of `$name` in `text` to the corresponding capture
    /// group `name`.
    ///
    /// `name` may be an integer corresponding to the index of the
    /// capture group (counted by order of opening parenthesis where `0` is the
    /// entire match) or it can be a name (consisting of letters, digits or
    /// underscores) corresponding to a named capture group.
    ///
    /// If `name` isn't a valid capture group (whether the name doesn't exist or
    /// isn't a valid index), then it is replaced with the empty string.
    ///
    /// To write a literal `$` use `$$`.
    pub fn expand(&self, text: &str) -> String {
        const REPLACE_EXPAND: &'static str = r"(?x)
          (?P<before>^|\b|[^$]) # Ignore `$$name`.
          \$
          (?P<name> # Match the actual capture name. Can be...
            [0-9]+  # A sequence of digits (for indexed captures), or...
            |
            [_a-zA-Z][_0-9a-zA-Z]* # A name for named captures.
          )
        ";
        // How evil can you get?
        let re = Regex::new(REPLACE_EXPAND).unwrap();
        let text = re.replace_all(text, |refs: &Captures| -> String {
            let before = refs.name("before").unwrap_or("");
            let name = refs.name("name").unwrap_or("");
            format!("{}{}", before, match name.parse::<usize>() {
                Err(_) => self.name(name).unwrap_or("").to_owned(),
                Ok(i) => self.at(i).unwrap_or("").to_owned(),
            })
        });
        let re = Regex::new(r"\$\$").unwrap();
        re.replace_all(&text, NoExpand("$"))
    }

    /// Returns the number of captured groups.
    #[inline]
    pub fn len(&self) -> usize {
        self.caps.len() / 2
    }

    /// Returns true if and only if there are no captured groups.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
    type Output = str;

    fn index(&self, i: usize) -> &str {
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
    type Output = str;

    fn index<'a>(&'a self, name: &'i str) -> &'a str {
        self.name(name).unwrap_or_else(|| panic!("no group named '{}'", name))
    }
}

/// An iterator over capture groups for a particular match of a regular
/// expression.
///
/// `'c` is the lifetime of the captures.
pub struct SubCaptures<'c> {
    idx: usize,
    caps: &'c Captures<'c>,
}

impl<'c> Iterator for SubCaptures<'c> {
    type Item = Option<&'c str>;

    fn next(&mut self) -> Option<Option<&'c str>> {
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
/// Positions are byte indices in terms of the original string matched.
///
/// `'c` is the lifetime of the captures.
pub struct SubCapturesPos<'c> {
    idx: usize,
    caps: &'c [Option<usize>]
}

impl<'c> Iterator for SubCapturesPos<'c> {
    type Item = Option<(usize, usize)>;

    fn next(&mut self) -> Option<Option<(usize, usize)>> {
        if self.idx >= self.caps.len() {
            return None
        }
        let r = match (self.caps[self.idx], self.caps[self.idx + 1]) {
            (Some(s), Some(e)) => Some((s, e)),
            (None, None) => None,
            _ => unreachable!()
        };
        self.idx += 2;
        Some(r)
    }
}

/// An Iterator over named capture groups as a tuple with the group
/// name and the value.
///
/// `'c` is the lifetime of the captures.
pub struct SubCapturesNamed<'c> {
    caps: &'c Captures<'c>,
    names: NamedGroupsIter<'c>,
}

impl<'c> Iterator for SubCapturesNamed<'c> {
    type Item = (&'c str, Option<&'c str>);

    fn next(&mut self) -> Option<(&'c str, Option<&'c str>)> {
        self.names.next().map(|(name, pos)| (name, self.caps.at(pos)))
    }
}

/// An iterator that yields all non-overlapping capture groups matching a
/// particular regular expression.
///
/// The iterator stops when no more matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the matched string.
pub struct FindCaptures<'r, 't> {
    re: &'r Regex,
    text: &'t str,
    last_end: usize,
    last_match: Option<usize>,
}

impl<'r, 't> Iterator for FindCaptures<'r, 't> {
    type Item = Captures<'t>;

    fn next(&mut self) -> Option<Captures<'t>> {
        if self.last_end > self.text.len() {
            return None
        }

        let mut caps = self.re.alloc_captures();
        if !self.re.exec(&mut caps, self.text, self.last_end) {
            return None
        }
        let (s, e) = (caps[0].unwrap(), caps[1].unwrap());

        // Don't accept empty matches immediately following a match.
        // i.e., no infinite loops please.
        if e == s && Some(self.last_end) == self.last_match {
            if self.last_end >= self.text.len() {
                return None;
            }
            self.last_end += self.text[self.last_end..].chars()
                                 .next().unwrap().len_utf8();
            return self.next();
        }
        self.last_end = e;
        self.last_match = Some(self.last_end);
        Some(Captures {
            text: self.text,
            caps: caps,
            named_groups: NamedGroups::from_regex(self.re),
        })
    }
}

/// An iterator over all non-overlapping matches for a particular string.
///
/// The iterator yields a tuple of integers corresponding to the start and end
/// of the match. The indices are byte offsets. The iterator stops when no more
/// matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the matched string.
pub struct FindMatches<'r, 't> {
    re: &'r Regex,
    text: &'t str,
    last_end: usize,
    last_match: Option<usize>,
}

impl<'r, 't> Iterator for FindMatches<'r, 't> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        if self.last_end > self.text.len() {
            return None
        }

        let mut caps = [None, None];
        if !self.re.exec(&mut caps, self.text, self.last_end) {
            return None;
        }
        let (s, e) = (caps[0].unwrap(), caps[1].unwrap());

        // Don't accept empty matches immediately following a match.
        // i.e., no infinite loops please.
        if e == s && Some(self.last_end) == self.last_match {
            if self.last_end >= self.text.len() {
                return None;
            }
            self.last_end += self.text[self.last_end..].chars()
                                 .next().unwrap().len_utf8();
            return self.next();
        }
        self.last_end = e;
        self.last_match = Some(self.last_end);
        Some((s, e))
    }
}

#[cfg(feature = "pattern")]
pub struct RegexSearcher<'r, 't> {
    it: FindMatches<'r, 't>,
    last_step_end: usize,
    next_match: Option<(usize, usize)>,
}

#[cfg(feature = "pattern")]
impl<'r, 't> Pattern<'t> for &'r Regex {
    type Searcher = RegexSearcher<'r, 't>;

    fn into_searcher(self, haystack: &'t str) -> RegexSearcher<'r, 't> {
        RegexSearcher {
            it: self.find_iter(haystack),
            last_step_end: 0,
            next_match: None,
        }
    }
}

#[cfg(feature = "pattern")]
unsafe impl<'r, 't> Searcher<'t> for RegexSearcher<'r, 't> {
    #[inline]
    fn haystack(&self) -> &'t str {
        self.it.text
    }

    #[inline]
    fn next(&mut self) -> SearchStep {
        if let Some((s, e)) = self.next_match {
            self.next_match = None;
            self.last_step_end = e;
            return SearchStep::Match(s, e);
        }
        match self.it.next() {
            None => {
                if self.last_step_end < self.haystack().len() {
                    let last = self.last_step_end;
                    self.last_step_end = self.haystack().len();
                    SearchStep::Reject(last, self.haystack().len())
                } else {
                    SearchStep::Done
                }
            }
            Some((s, e)) => {
                if s == self.last_step_end {
                    self.last_step_end = e;
                    SearchStep::Match(s, e)
                } else {
                    self.next_match = Some((s, e));
                    let last = self.last_step_end;
                    self.last_step_end = s;
                    SearchStep::Reject(last, s)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::{NoExpand, Regex};

    #[test]
    fn test_simple_expand() {
        let re = Regex::new(r"(\w) (\w)").unwrap();
        assert_eq!(re.replace_all("a b", "$2 $1"), "b a");
    }

    #[test]
    fn test_literal_dollar() {
        let re = Regex::new(r"(\w+) (\w+)").unwrap();
        assert_eq!(re.replace_all("a b", "$1"), "a");
        assert_eq!(re.replace_all("a b", "$$1"), "$1");  // $$ should become a $
        assert_eq!(re.replace_all("a b", "$2 $$c $1"), "b $c a");
    }

    #[test]
    fn test_no_expand() {
        let re = Regex::new(r"(\w+)").unwrap();
        assert_eq!(re.replace_all("a", NoExpand("$$1")), "$$1");
        assert_eq!(re.replace_all("a", NoExpand("$1")), "$1");
    }

    #[test]
    fn test_capture_names() {
        let re = Regex::new(r"(.)(?P<a>.)").unwrap();
        assert_eq!(re.capture_names().size_hint(), (3, Some(3)));
        assert_eq!(re.capture_names().collect::<Vec<_>>(), [None, None, Some("a")]);
    }

    #[test]
    fn test_cap_index() {
        let re = Regex::new(r"^(?P<name>.+)$").unwrap();
        let cap = re.captures("abc").unwrap();
        assert_eq!(&cap[0], "abc");
        assert_eq!(&cap[1], "abc");
        assert_eq!(&cap["name"], "abc");
    }

    #[test]
    #[should_panic]
    #[cfg_attr(all(target_env = "msvc", target_pointer_width = "32"), ignore)]
    fn test_cap_index_panic_usize() {
        let re = Regex::new(r"^(?P<name>.+)$").unwrap();
        let cap = re.captures("abc").unwrap();
        let _ = cap[2];
    }

    #[test]
    #[should_panic]
    #[cfg_attr(all(target_env = "msvc", target_pointer_width = "32"), ignore)]
    fn test_cap_index_panic_name() {
        let re = Regex::new(r"^(?P<name>.+)$").unwrap();
        let cap = re.captures("abc").unwrap();
        let _ = cap["bad name"];
    }

    #[test]
    fn test_cap_index_lifetime() {
        // This is a test of whether the types on `caps["..."]` are general
        // enough.  If not, this will fail to typecheck.
        fn inner(s: &str) -> usize {
            let re = Regex::new(r"(?P<number>\d+)").unwrap();
            let caps = re.captures(s).unwrap();
            caps["number"].len()
        }
        assert_eq!(inner("123"), 3);
    }
}
