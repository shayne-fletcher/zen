all: target/debug/lib_term.a
	g++ -std=c++14 src/main.cpp -I . -L ./target/debug -lterm -o run
	LD_LIBRARY_PATH=./target/debug/ ./run

target/debug/lib_term.a: src/lib.rs src/arith.rs ./Cargo.toml
	cargo build
