fn main() {
    cxx_build::bridge("src/arith_final_tagless.rs")
        .file("src/cpp_repr.cpp")
        .flag_if_supported("-std=c++14")
        .compile("arith_final_tagless");
}
