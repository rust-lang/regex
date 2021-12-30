RS_SRC := $(shell find src -type f) Cargo.toml

../target/release/librure.a ../target/release/librure.dylib &: $(RS_SRC)
	cargo build --release

install: ../target/release/librure.a ../target/release/librure.dylib
	cp ../target/release/librure.a /usr/local/lib/
	cp ../target/release/librure.dylib /usr/local/lib/
	cp include/rure.h /usr/local/include
