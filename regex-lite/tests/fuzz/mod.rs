#[test]
fn captures_wrong_order() {
    let data = include_bytes!(
        "testdata/crash-a886ce2b0d64963f1232f9b08b8c9ad4740c26f5"
    );
    let _ = run(data);
}

#[test]
fn captures_wrong_order_min() {
    let data = include_bytes!(
        "testdata/minimized-from-298f84f9dbb2589cb9938a63334fa4083b609f34"
    );
    let _ = run(data);
}

// This is the fuzz target function. We duplicate it here since this is the
// thing we use to interpret the data. It is ultimately what we want to
// succeed.
fn run(data: &[u8]) -> Option<()> {
    if data.len() < 2 {
        return None;
    }
    let mut split_at = usize::from(data[0]);
    let data = std::str::from_utf8(&data[1..]).ok()?;
    // Split data into a regex and haystack to search.
    let len = usize::try_from(data.chars().count()).ok()?;
    split_at = std::cmp::max(split_at, 1) % len;
    let char_index = data.char_indices().nth(split_at)?.0;
    let (pattern, input) = data.split_at(char_index);
    let re = regex_lite::Regex::new(pattern).ok()?;
    re.is_match(input);
    Some(())
}
