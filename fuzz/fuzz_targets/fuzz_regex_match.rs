#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if data.len() < 2 {
        return;
    }
    let split_point = data[0] as usize;
    if let Ok(data) = std::str::from_utf8(&data[1..]) {
        use std::cmp::max;
        // split data into regular expression and actual input to search through
        let len = data.chars().count();
        let split_off_point = max(split_point, 1) % len as usize;
        let char_index = data.char_indices().nth(split_off_point);
        if let Some((char_index, _)) = char_index {
            let (pattern, input) = data.split_at(char_index);
            let result =
                regex::RegexBuilder::new(pattern).size_limit(1 << 18).build();
            if let Ok(re) = result {
                re.is_match(input);
            }
        }
    }
});
