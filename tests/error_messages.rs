// See: https://github.com/rust-lang/regex/issues/545
#[test]
fn repetition_quantifier_expects_a_valid_decimal() {
    assert_panic_message(r"\\u{[^}]*}", r#"
regex parse error:
    \\u{[^}]*}
        ^
error: repetition quantifier expects a valid decimal
"#);
}

fn assert_panic_message(regex: &str, expected_msg: &str) -> () {
    let result = regex_new!(regex);
    match result {
        Ok(_) => panic!("Regular expression should have panicked"),
        Err(regex::Error::Syntax(msg)) => assert_eq!(msg, expected_msg.trim()),
        _ => panic!("Unexpected error received")
    }
}
