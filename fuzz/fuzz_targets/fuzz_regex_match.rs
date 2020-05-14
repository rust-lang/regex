#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(data) = std::str::from_utf8(data) {
        // split data into regular expression and actual input to search through
        use std::cmp::max;
        let len = data.chars().count();
        let split_off_point = max(len / 5, 1) as usize;
        let char_index = data.char_indices().nth(split_off_point);

        if let Some((char_index, _)) = char_index {
            let (pattern, input) = data.split_at(char_index);
            if let Ok(re) = regex::Regex::new(pattern) {
                re.is_match(input);
            }
        }
    }
});
