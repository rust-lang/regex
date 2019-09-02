#!/bin/sh

# vim: tabstop=2 shiftwidth=2 softtabstop=2

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

# Check formatting, but make sure we use the stable version of rustfmt.
if [ "$TRAVIS_RUST_VERSION" = "stable" ]; then
  rustup component add rustfmt
  cargo fmt --all -- --check
fi

# Only run the full test suite on one job to keep build times lower.
if [ "$TRAVIS_RUST_VERSION" = "stable" ]; then
  ./test

  # Run the random tests in release mode, as this is faster.
  RUST_REGEX_RANDOM_TEST=1 cargo test --release --verbose --test crates-regex
else
  cargo test --verbose --test default
fi

# Run a test that confirms the shootout benchmarks are correct.
ci/run-shootout-test

# Run tests on regex-syntax crate.
cargo doc --verbose --manifest-path regex-syntax/Cargo.toml
# Only run the full test suite on one job, to conserve resources.
if [ "$TRAVIS_RUST_VERSION" = "stable" ]; then
  (cd regex-syntax && ./test)
else
  cargo test --verbose --manifest-path regex-syntax/Cargo.toml
fi

# Run tests on regex-capi crate.
ci/test-regex-capi

# Make sure benchmarks compile. Don't run them though because they take a
# very long time. Also, check that we can build the regex-debug tool.
if [ "$TRAVIS_RUST_VERSION" = "nightly" ]; then
  cargo build --verbose --manifest-path regex-debug/Cargo.toml
  (cd bench && ./run rust --no-run --verbose)

  # Test minimal versions.
  #
  # rand has started putting the minimal version check in their CI, so we
  # should be able to re-enable this soon. This will require upgrading to
  # rand 0.7, which breaks our MSRV since it relies on Rust 2018 features in
  # order to read the Cargo.toml.
  # cargo +nightly generate-lockfile -Z minimal-versions
  # cargo build --verbose
  # cargo test --verbose
fi
