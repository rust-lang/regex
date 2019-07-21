#!/bin/sh

# This is the main CI script for testing the regex crate and its sub-crates.

set -ex
MSRV="1.28.0"

# Builds the regex crate and runs tests.
cargo build --verbose
cargo doc --verbose

# If we're testing on an older version of Rust, then only check that we
# can build the crate. This is because the dev dependencies might be updated
# more frequently, and therefore might require a newer version of Rust.
#
# This isn't ideal. It's a compromise.
if [ "$TRAVIS_RUST_VERSION" = "$MSRV" ]; then
  exit
fi

# Run tests. If we have nightly, then enable our nightly features.
# Right now there are no nightly features, but that may change in the
# future.
CARGO_TEST_EXTRA_FLAGS=""
if [ "$TRAVIS_RUST_VERSION" = "nightly" ]; then
  CARGO_TEST_EXTRA_FLAGS=""
fi
cargo test --verbose ${CARGO_TEST_EXTRA_FLAGS}

# Run the random tests in release mode, as this is faster.
RUST_REGEX_RANDOM_TEST=1 \
    cargo test --release --verbose \
    ${CARGO_TEST_EXTRA_FLAGS} --test crates-regex

# Run a test that confirms the shootout benchmarks are correct.
ci/run-shootout-test

# Run tests on regex-syntax crate.
cargo test --verbose --manifest-path regex-syntax/Cargo.toml
cargo doc --verbose --manifest-path regex-syntax/Cargo.toml

# Run tests on regex-capi crate.
ci/test-regex-capi

# Make sure benchmarks compile. Don't run them though because they take a
# very long time. Also, check that we can build the regex-debug tool.
if [ "$TRAVIS_RUST_VERSION" = "nightly" ]; then
  cargo build --verbose --manifest-path regex-debug/Cargo.toml
  (cd bench && ./run rust --no-run --verbose)

  # Test minimal versions.
  #
  # For now, we remove this check, because it doesn't seem possible to convince
  # some maintainers of *core* crates that this is a worthwhile test to add.
  # In particular, this test uncovers any *incorrect* dependency specification
  # in the chain of dependencies.
  #
  # We might consider figuring out how to migrate off of rand in order to get
  # this check working. (This will be hard, since it either requires dropping
  # quickcheck or migrating quickcheck off of rand, which is just probably
  # not practical.)
  #
  # So frustrating.
  # cargo +nightly generate-lockfile -Z minimal-versions
  # cargo build --verbose
  # cargo test --verbose
fi
