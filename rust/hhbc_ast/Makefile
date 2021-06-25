all: target/debug/lib_ast.a
	g++ -std=c++14 src/main.cpp -I . -L ./target/debug -last -o run
	LD_LIBRARY_PATH=./target/debug/ ./run

target/debug/lib_ast.a: src/lib.rs src/ast.rs ./Cargo.toml
	cargo build
