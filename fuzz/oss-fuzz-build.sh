#!/bin/bash -eu
cd $SRC/regex
cargo fuzz build -O 
cp fuzz/target/x86_64-unknown-linux-gnu/release/fuzz_regex_match $OUT/
