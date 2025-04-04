all: target/debug/libcpp_repr.a target/debug/libarith_final_tagless.a
	g++ -std=c++14 src/main.cpp -I .. -I target/cxxbridge -L ./target/debug/ -larith_final_tagless -ldl -lcpp_repr -lpthread -o run
	LD_LIBRARY_PATH=./target/debug/ ./run

target/debug:
	mkdir -p target/debug

target/debug/libcpp_repr.a: target/debug include/cpp_repr.hpp src/cpp_repr.cpp
	g++ -std=c++14 -I .. -c src/cpp_repr.cpp -o target/debug/cpp_repr.o
	ar rvs target/debug/libcpp_repr.a target/debug/cpp_repr.o

target/debug/libarith_final_tagless.a: src/lib.rs src/arith_final_tagless.rs src/cpp_repr.cpp include/cpp_repr.hpp ./Cargo.toml
	cargo build

clean:
	rm -rf target
	rm -rf run
