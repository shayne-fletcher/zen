all: target/debug/libarith_final_tagless.a
	g++ -std=c++14 src/main.cpp -I .. -I target/cxxbridge -L ./target/debug/ -larith_final_tagless -o run
	LD_LIBRARY_PATH=./target/debug/ ./run

target/debug/libarith_final_tagless.a: src/lib.rs src/arith_final_tagless.rs src/cpp_repr.cpp include/cpp_repr.hpp ./Cargo.toml
	cargo build

clean:
	rm -rf target
	rm -rf run
