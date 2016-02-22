extern crate regex;

use regex::RegexSet;

fn main() {
    let res = &[
        "abc",
        "xyzz",
        "^[ga-fh-z]+$",
    ];
    let text = "abcggggggggxyz";
    let set = RegexSet::new(res).unwrap();
    println!("{:?}", set);
    let m = set.is_match("abcggggggggxyz");
    println!("match? {:?}", m);
    for mi in set.matches(text) {
        println!("{:?}", mi);
    }
}
