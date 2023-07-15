#!/bin/bash -eu

cd $SRC/regex
cargo fuzz build -O --debug-assertions

targets=(
  fuzz_regex_match
  fuzz_regex_lite_match
  fuzz_regex_automata_deserialize_dense_dfa
  fuzz_regex_automata_deserialize_sparse_dfa
  ast_roundtrip
  ast_fuzz_match
  ast_fuzz_regex
  ast_fuzz_match_bytes
)
for target in "${targets[@]}"; do
  cp "fuzz/target/x86_64-unknown-linux-gnu/release/${target}" "${OUT}/"
  if [[ "$target" == ast_* ]]; then
    cp fuzz/ast-fuzzers.options "${OUT}/${target}.options"
  fi
done
