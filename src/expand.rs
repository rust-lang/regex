use std::str;

use memchr::memchr;

use bytes::Captures;

pub fn expand(caps: &Captures, mut replacement: &[u8], dst: &mut Vec<u8>) {
    while !replacement.is_empty() {
        match memchr(b'$', replacement) {
            None => break,
            Some(i) => {
                dst.extend(&replacement[..i]);
                replacement = &replacement[i..];
            }
        }
        if replacement.get(1).map_or(false, |&b| b == b'$') {
            dst.push(b'$');
            replacement = &replacement[2..];
            continue;
        }
        debug_assert!(!replacement.is_empty());
        let cap_ref = match find_cap_ref(replacement) {
            Some(cap_ref) => cap_ref,
            None => {
                dst.push(b'$');
                replacement = &replacement[1..];
                continue;
            }
        };
        replacement = cap_ref.rest;
        match cap_ref.cap {
            Ref::Number(i) => dst.extend(caps.at(i).unwrap_or(b"")),
            Ref::Named(name) => dst.extend(caps.name(name).unwrap_or(b"")),
        }
    }
    dst.extend(replacement);
}

struct CaptureRef<'a> {
    rest: &'a [u8],
    cap: Ref<'a>,
}

enum Ref<'a> {
    Named(&'a str),
    Number(usize),
}

fn find_cap_ref(mut replacement: &[u8]) -> Option<CaptureRef> {
    if replacement.len() <= 1 || replacement[0] != b'$' {
        return None;
    }
    let mut brace = false;
    replacement = &replacement[1..];
    if replacement[0] == b'{' {
        brace = true;
        replacement = &replacement[1..];
    }
    let mut cap_end = 0;
    while replacement.get(cap_end).map_or(false, is_valid_cap_letter) {
        cap_end += 1;
    }
    if cap_end == 0 {
        return None;
    }
    // We just verified that the range 0..cap_end is valid ASCII, so it must
    // therefore be valid UTF-8. If we really cared, we could avoid this UTF-8
    // check with either unsafe or by parsing the number straight from &[u8].
    let cap = str::from_utf8(&replacement[..cap_end])
                  .ok().expect("valid UTF-8 capture name");
    if brace {
        if !replacement.get(cap_end).map_or(false, |&b| b == b'}') {
            return None;
        }
        cap_end += 1;
    }
    Some(CaptureRef {
        rest: &replacement[cap_end..],
        cap: match cap.parse::<u32>() {
            Ok(i) => Ref::Number(i as usize),
            Err(_) => Ref::Named(cap),
        },
    })
}

fn is_valid_cap_letter(b: &u8) -> bool {
    match *b {
        b'0' ... b'9' | b'a' ... b'z' | b'A' ... b'Z' | b'_' => true,
        _ => false,
    }
}
