#!/bin/bash -eu

cd $SRC/regex
cargo fuzz build -O --debug-assertions

targets=$(cargo fuzz list)
for target in "${targets[@]}"; do
  cp "fuzz/target/x86_64-unknown-linux-gnu/release/${target}" "${OUT}/"
  if [[ "$target" == ast_* ]]; then
    cp fuzz/ast-fuzzers.options "${OUT}/${target}.options"
  fi
done
