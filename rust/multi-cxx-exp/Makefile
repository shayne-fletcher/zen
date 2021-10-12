all: target/debug/libmulti_cxx.a
	g++ -std=c++14 src/main.cpp -I .. -I target/cxxbridge -L ./target/debug/ -lmulti_cxx -o run
	LD_LIBRARY_PATH=./target/debug/ ./run

target/debug/libmulti_cxx.a: src/lib.rs src/thing.rs src/multi_cxx1.rs src/multi_cxx2.rs ./Cargo.toml
	cargo build

clean:
	rm -rf target
	rm -rf run
