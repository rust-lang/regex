use debugger_test::debugger_test;

#[inline(never)]
fn __break() {}

#[debugger_test(
    debugger = "cdb",
    commands = r#"
.nvlist
dv
dx re
dx captures
g
dx m1
dx m2
dx m3
dx m4
"#,
    expected_statements = r#"
re               : { text="^(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})$" } [Type: regex::re_unicode::Regex]
[<Raw View>]     [Type: regex::re_unicode::Regex]
[Reference count] : 0x2 [Type: core::sync::atomic::AtomicUsize]
[Weak reference count] : 0x1 [Type: core::sync::atomic::AtomicUsize]
[+0xc00] res              : { len=0x1 } [Type: alloc::vec::Vec<alloc::string::String,alloc::alloc::Global>]
[+0x000] nfa              [Type: regex::prog::Program]
[+0x320] dfa              [Type: regex::prog::Program]
[+0x640] dfa_reverse      [Type: regex::prog::Program]
[+0x960] suffixes         [Type: regex::literal::imp::LiteralSearcher]
[+0xc18] ac               : None [Type: enum$<core::option::Option<aho_corasick::ahocorasick::AhoCorasick<u32> >, 0, 1, Some>]
[+0xda0] match_type       : Dfa [Type: enum$<regex::exec::MatchType>]

captures         : { named_groups=0x3 } [Type: regex::re_unicode::Captures]
[<Raw View>]     [Type: regex::re_unicode::Captures]
[text]           : "2020-10-15" [Type: str]
pattern:\[named_groups\]   : \{ len=0x3 \} \[Type: .*\]
pattern:\[0\]              : .* : "2020-10-15" \[Type: char \*\]
pattern:\[1\]              : .* : "2020" \[Type: char \*\]
pattern:\[2\]              : .* : "10" \[Type: char \*\]
pattern:\[3\]              : .* : "15" \[Type: char \*\]

m1               : "2020-10-15" [Type: regex::re_unicode::Match]
[<Raw View>]     [Type: regex::re_unicode::Match]
[text]           : "2020-10-15" [Type: str]
[match_text]     : "2020-10-15"
[start]          : 0 [Type: unsigned __int64]
[end]            : 10 [Type: unsigned __int64]

m2               : "2020" [Type: regex::re_unicode::Match]
[<Raw View>]     [Type: regex::re_unicode::Match]
[text]           : "2020-10-15" [Type: str]
[match_text]     : "2020"
[start]          : 0 [Type: unsigned __int64]
[end]            : 4 [Type: unsigned __int64]

m3               : "10" [Type: regex::re_unicode::Match]
[<Raw View>]     [Type: regex::re_unicode::Match]
[text]           : "2020-10-15" [Type: str]
[match_text]     : "10"
[start]          : 5 [Type: unsigned __int64]
[end]            : 7 [Type: unsigned __int64]

m4               : "15" [Type: regex::re_unicode::Match]
[<Raw View>]     [Type: regex::re_unicode::Match]
[text]           : "2020-10-15" [Type: str]
[match_text]     : "15"
[start]          : 8 [Type: unsigned __int64]
[end]            : 10 [Type: unsigned __int64]
"#
)]
fn test_debugger_visualizer() {
    let re = regex::Regex::new(
        r"^(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})$",
    )
    .unwrap();
    let text = "2020-10-15";

    let captures = re.captures(text).unwrap();
    let matches = captures
        .iter()
        .filter_map(|capture| capture)
        .collect::<Vec<regex::Match>>();
    assert_eq!(4, matches.len());
    __break(); // #break

    let m1 = matches[0];
    assert_eq!("2020-10-15", m1.as_str());

    let m2 = matches[1];
    assert_eq!("2020", m2.as_str());

    let m3 = matches[2];
    assert_eq!("10", m3.as_str());

    let m4 = matches[3];
    assert_eq!("15", m4.as_str());
    __break(); // #break
}

#[debugger_test(
    debugger = "cdb",
    commands = r#"
.nvlist
dv
dx re
dx captures
g
dx m1
dx m2
dx m3
dx m4
"#,
    expected_statements = r#"
re               : { text="^(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})$" } [Type: regex::re_bytes::Regex]
[<Raw View>]     [Type: regex::re_bytes::Regex]
[Reference count] : 0x2 [Type: core::sync::atomic::AtomicUsize]
[Weak reference count] : 0x1 [Type: core::sync::atomic::AtomicUsize]
[+0xc00] res              : { len=0x1 } [Type: alloc::vec::Vec<alloc::string::String,alloc::alloc::Global>]
[+0x000] nfa              [Type: regex::prog::Program]
[+0x320] dfa              [Type: regex::prog::Program]
[+0x640] dfa_reverse      [Type: regex::prog::Program]
[+0x960] suffixes         [Type: regex::literal::imp::LiteralSearcher]
[+0xc18] ac               : None [Type: enum$<core::option::Option<aho_corasick::ahocorasick::AhoCorasick<u32> >, 0, 1, Some>]
[+0xda0] match_type       : Dfa [Type: enum$<regex::exec::MatchType>]

captures         : { named_groups=0x3 } [Type: regex::re_bytes::Captures]
[<Raw View>]     [Type: regex::re_bytes::Captures]
[text]           : { len=0xa } [Type: slice$<u8>]
pattern:\[named_groups\]   : \{ len=0x3 \} \[Type: .*\]
pattern:\[0\]              : .* : "2020-10-15" \[Type: char \*\]
pattern:\[1\]              : .* : "2020" \[Type: char \*\]
pattern:\[2\]              : .* : "10" \[Type: char \*\]
pattern:\[3\]              : .* : "15" \[Type: char \*\]

m1               : "2020-10-15" [Type: regex::re_bytes::Match]
[<Raw View>]     [Type: regex::re_bytes::Match]
[text]           : { len=0xa } [Type: slice$<u8>]
[match_text]     : "2020-10-15"
[start]          : 0 [Type: unsigned __int64]
[end]            : 10 [Type: unsigned __int64]

m2               : "2020" [Type: regex::re_bytes::Match]
[<Raw View>]     [Type: regex::re_bytes::Match]
[text]           : { len=0xa } [Type: slice$<u8>]
[match_text]     : "2020"
[start]          : 0 [Type: unsigned __int64]
[end]            : 4 [Type: unsigned __int64]

m3               : "10" [Type: regex::re_bytes::Match]
[<Raw View>]     [Type: regex::re_bytes::Match]
[text]           : { len=0xa } [Type: slice$<u8>]
[match_text]     : "10"
[start]          : 5 [Type: unsigned __int64]
[end]            : 7 [Type: unsigned __int64]

m4               : "15" [Type: regex::re_bytes::Match]
[<Raw View>]     [Type: regex::re_bytes::Match]
[text]           : { len=0xa } [Type: slice$<u8>]
[match_text]     : "15"
[start]          : 8 [Type: unsigned __int64]
[end]            : 10 [Type: unsigned __int64]
"#
)]
fn test_bytes_debugger_visualizer() {
    let re = regex::bytes::Regex::new(
        r"^(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})$",
    )
    .unwrap();
    let text = b"2020-10-15";

    let captures = re.captures(text).unwrap();
    let matches = captures
        .iter()
        .filter_map(|capture| capture)
        .collect::<Vec<regex::bytes::Match>>();
    assert_eq!(4, matches.len());
    __break(); // #break

    let m1 = matches[0];
    assert_eq!(b"2020-10-15", m1.as_bytes());

    let m2 = matches[1];
    assert_eq!(b"2020", m2.as_bytes());

    let m3 = matches[2];
    assert_eq!(b"10", m3.as_bytes());

    let m4 = matches[3];
    assert_eq!(b"15", m4.as_bytes());

    __break(); // #break
}
