regex
=====

A Rust library for parsing, compiling, and executing regular expressions.
This particular implementation of regular expressions guarantees execution
in linear time with respect to the size of the regular expression and
search text. Much of the syntax and implementation is inspired by
[RE2](https://github.com/google/re2).

[![Build Status](https://travis-ci.org/rust-lang-nursery/regex.svg?branch=master)](https://travis-ci.org/rust-lang-nursery/regex)
[![Build status](https://ci.appveyor.com/api/projects/status/22g48bo866qr4u77?svg=true)](https://ci.appveyor.com/project/alexcrichton/regex)
[![](http://meritbadge.herokuapp.com/regex)](https://crates.io/crates/regex)

### Documentation

[Module documentation with examples](https://doc.rust-lang.org/regex).
The module documentation also include a comprehensive description of the syntax
supported.

Documentation with examples for the various matching functions and iterators
can be found on the
[`Regex` type](https://doc.rust-lang.org/regex/regex/enum.Regex.html).


### Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
regex = "0.1"
```

and this to your crate root:

```rust
extern crate regex;
```

Here's a simple example that matches a date in YYYY-MM-DD format and prints the
year, month and day:

```rust
extern crate regex;

use regex::Regex;

fn main() {
    let re = Regex::new(r"(?x)
(?P<year>\d{4})  # the year
-
(?P<month>\d{2}) # the month
-
(?P<day>\d{2})   # the day
").unwrap();
    let caps = re.captures("2010-03-14").unwrap();

    assert_eq!("2010", caps.name("year").unwrap());
    assert_eq!("03", caps.name("month").unwrap());
    assert_eq!("14", caps.name("day").unwrap());
}
```

If you have lots of dates in text that you'd like to iterate over, then it's
easy to adapt the above example with an iterator:

```rust
extern crate regex;

use regex::Regex;

const TO_SEARCH: &'static str = "
On 2010-03-14, foo happened. On 2014-10-14, bar happened.
";

fn main() {
    let re = Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap();

    for caps in re.captures_iter(TO_SEARCH) {
        // Note that all of the unwraps are actually OK for this regex
        // because the only way for the regex to match is if all of the
        // capture groups match. This is not true in general though!
        println!("year: {}, month: {}, day: {}",
                 caps.at(1).unwrap(),
                 caps.at(2).unwrap(),
                 caps.at(3).unwrap());
    }
}
```

This example outputs:

```
year: 2010, month: 03, day: 14
year: 2014, month: 10, day: 14
```

### Usage: `regex!` compiler plugin

The `regex!` compiler plugin will compile your regexes at compile time. **This
only works with a nightly compiler.**
The
[documentation explains the trade
offs](https://doc.rust-lang.org/regex/regex/index.html#the-regex!-macro).

Here is a small example:

```rust
#![feature(plugin)]

#![plugin(regex_macros)]
extern crate regex;

fn main() {
    let re = regex!(r"(\d{4})-(\d{2})-(\d{2})");
    let caps = re.captures("2010-03-14").unwrap();

    assert_eq!("2010", caps.at(1).unwrap());
    assert_eq!("03", caps.at(2).unwrap());
    assert_eq!("14", caps.at(3).unwrap());
}
```

Notice that we never `unwrap` the result of `regex!`. This is because your
*program* won't compile if the regex doesn't compile. (Try `regex!("(")`.)

### Usage: a regular expression parser

This repository contains a crate that provides a well tested regular expression
parser and abstract syntax. It provides no facilities for compilation or
execution. This may be useful if you're implementing your own regex engine or
otherwise need to do analysis on the syntax of a regular expression. It is
otherwise not recommended for general use.

[Documentation for `regex-syntax` with
examples](https://doc.rust-lang.org/regex/regex_syntax/index.html).

# License

`regex` is primarily distributed under the terms of both the MIT license and
the Apache License (Version 2.0), with portions covered by various BSD-like
licenses.

See LICENSE-APACHE, and LICENSE-MIT for details.
