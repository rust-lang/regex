#[test]
fn empty_regex_empty_match() {
    let re = regex!("");
    assert_eq!(vec![(0, 0)], findall!(re, ""));
}

#[test]
fn empty_regex_nonempty_match() {
    let re = regex!("");
    assert_eq!(vec![(0, 0), (1, 1), (2, 2), (3, 3)], findall!(re, "abc"));
}

#[test]
fn one_zero_length_match() {
    let re = regex!(r"\d*");
    assert_eq!(vec![(0, 0), (1, 2), (3, 4)], findall!(re, "a1b2"));
}

#[test]
fn many_zero_length_match() {
    let re = regex!(r"\d*");
    assert_eq!(vec![(0, 0), (1, 2), (3, 3), (4, 4), (5, 6)],
               findall!(re, "a1bbb2"));
}

#[test]
fn many_sequential_zero_length_match() {
    let re = regex!(r"\d?");
    assert_eq!(vec![(0, 0), (1, 2), (2, 3), (4, 5), (6, 6)],
               findall!(re, "a12b3c"));
}

#[test]
fn quoted_bracket_set() {
    let re = regex!(r"([\x{5b}\x{5d}])");
    assert_eq!(vec![(0, 1), (1, 2)], findall!(re, "[]"));
    let re = regex!(r"([\[\]])");
    assert_eq!(vec![(0, 1), (1, 2)], findall!(re, "[]"));
}

#[test]
fn first_range_starts_with_left_bracket() {
    let re = regex!(r"([[-z])");
    assert_eq!(vec![(0, 1), (1, 2)], findall!(re, "[]"));
}

#[test]
fn range_ends_with_escape() {
    let re = regex!(r"([\[-\x{5d}])");
    assert_eq!(vec![(0, 1), (1, 2)], findall!(re, "[]"));
}

#[test]
fn empty_match_find_iter() {
    let re = regex!(r".*?");
    assert_eq!(vec![(0, 0), (1, 1), (2, 2), (3, 3)], findall!(re, "abc"));
}

#[test]
fn empty_match_captures_iter() {
    let re = regex!(r".*?");
    let ms: Vec<_> = re.captures_iter(text!("abc"))
                       .map(|c| c.get(0).unwrap())
                       .map(|m| (m.start(), m.end()))
                       .collect();
    assert_eq!(ms, vec![(0, 0), (1, 1), (2, 2), (3, 3)]);
}

#[test]
fn capture_names() {
    let re = regex!(r"(.)(?P<a>.)");
    assert_eq!(3, re.captures_len());
    assert_eq!((3, Some(3)), re.capture_names().size_hint());
    assert_eq!(vec![None, None, Some("a")],
               re.capture_names().collect::<Vec<_>>());
}

#[test]
fn regex_string() {
    assert_eq!(r"[a-zA-Z0-9]+", regex!(r"[a-zA-Z0-9]+").as_str());
    assert_eq!(r"[a-zA-Z0-9]+", &format!("{}", regex!(r"[a-zA-Z0-9]+")));
    assert_eq!(r"[a-zA-Z0-9]+", &format!("{:?}", regex!(r"[a-zA-Z0-9]+")));
}

#[test]
fn capture_index() {
    let re = regex!(r"^(?P<name>.+)$");
    let cap = re.captures(t!("abc")).unwrap();
    assert_eq!(&cap[0], t!("abc"));
    assert_eq!(&cap[1], t!("abc"));
    assert_eq!(&cap["name"], t!("abc"));
}

#[test]
#[should_panic]
#[cfg_attr(all(target_env = "msvc", target_pointer_width = "32"), ignore)]
fn capture_index_panic_usize() {
    let re = regex!(r"^(?P<name>.+)$");
    let cap = re.captures(t!("abc")).unwrap();
    let _ = cap[2];
}

#[test]
#[should_panic]
#[cfg_attr(all(target_env = "msvc", target_pointer_width = "32"), ignore)]
fn capture_index_panic_name() {
    let re = regex!(r"^(?P<name>.+)$");
    let cap = re.captures(t!("abc")).unwrap();
    let _ = cap["bad name"];
}

#[test]
fn capture_index_lifetime() {
    // This is a test of whether the types on `caps["..."]` are general
    // enough. If not, this will fail to typecheck.
    fn inner(s: &str) -> usize {
        let re = regex!(r"(?P<number>\d+)");
        let caps = re.captures(t!(s)).unwrap();
        caps["number"].len()
    }
    assert_eq!(3, inner("123"));
}

#[test]
fn capture_misc() {
    let re = regex!(r"(.)(?P<a>a)?(.)(?P<b>.)");
    let cap = re.captures(t!("abc")).unwrap();

    assert_eq!(5, cap.len());

    assert_eq!((0, 3), { let m = cap.get(0).unwrap(); (m.start(), m.end()) });
    assert_eq!(None, cap.get(2));
    assert_eq!((2, 3), { let m = cap.get(4).unwrap(); (m.start(), m.end()) });

    assert_eq!(t!("abc"), match_text!(cap.get(0).unwrap()));
    assert_eq!(None, cap.get(2));
    assert_eq!(t!("c"), match_text!(cap.get(4).unwrap()));

    assert_eq!(None, cap.name("a"));
    assert_eq!(t!("c"), match_text!(cap.name("b").unwrap()));
}

expand!(expand1, r"(?P<foo>\w+)", "abc", "$foo", "abc");
expand!(expand2, r"(?P<foo>\w+)", "abc", "$0", "abc");
expand!(expand3, r"(?P<foo>\w+)", "abc", "$1", "abc");
expand!(expand4, r"(?P<foo>\w+)", "abc", "$$1", "$1");
expand!(expand5, r"(?P<foo>\w+)", "abc", "$$foo", "$foo");
expand!(expand6, r"(?P<a>\w+)\s+(?P<b>\d+)",
        "abc 123", "$b$a", "123abc");
expand!(expand7, r"(?P<a>\w+)\s+(?P<b>\d+)",
        "abc 123", "z$bz$az", "z");
expand!(expand8, r"(?P<a>\w+)\s+(?P<b>\d+)",
        "abc 123", ".$b.$a.", ".123.abc.");
expand!(expand9, r"(?P<a>\w+)\s+(?P<b>\d+)",
        "abc 123", " $b $a ", " 123 abc ");
expand!(expand10, r"(?P<a>\w+)\s+(?P<b>\d+)",
        "abc 123", "$bz$az", "");

split!(split1, r"\s+", "a b\nc\td\n\t e",
       &[t!("a"), t!("b"), t!("c"), t!("d"), t!("e")]);
split!(split2, r"\b", "a b c",
       &[t!(""), t!("a"), t!(" "), t!("b"), t!(" "), t!("c")]);
split!(split3, r"a$", "a", &[t!("")]);
