#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let _ = run(data);
});

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
