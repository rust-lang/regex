RS_SRC := $(shell find src -type f) Cargo.toml

../target/release/librure.a ../target/release/librure.so &: $(RS_SRC)
	cargo build --release

install: ../target/release/librure.a ../target/release/librure.so
	cp ../target/release/librure.a /usr/local/lib/
	cp ../target/release/librure.so /usr/local/lib/
	cp include/rure.h /usr/local/include
