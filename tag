#!/usr/bin/env bash

execute=
if [ $1 = execute ]; then
  execute=" --execute"
elif [ $1 != dry-run ]; then
  echo "Usage: $(basename "$0") <execute | dry-run> crate ..." >&2
  exit 1
fi
shift

for crate; do
  if [ $crate = regex ]; then
    cargo release tag \
      --no-confirm \
      --sign-tag --tag-name '{{version}}' -p regex${execute}
  else
    cargo release tag \
      --no-confirm \
      --sign-tag --tag-name '{{prefix}}{{version}}' -p ${crate}${execute}
  fi
done
