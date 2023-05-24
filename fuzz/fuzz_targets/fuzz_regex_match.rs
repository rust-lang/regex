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
            // If the haystack is big, don't use it. The issue is that
            // the fuzzer is compiled with sanitizer options and it makes
            // everything pretty slow. This was put in here as a result of
            // getting timeout errors from OSS-fuzz. There's really nothing to
            // be done about them. Unicode word boundaries in the PikeVM are
            // slow. It is what it is.
            if input.len() >= 8 * (1 << 10) {
                return;
            }
            let result =
                regex::RegexBuilder::new(pattern).size_limit(1 << 18).build();
            if let Ok(re) = result {
                re.is_match(input);
            }
        }
    }
});
