fn main() {
    cxx_build::bridge("src/arith_final_tagless.rs")
        .flag_if_supported("-std=c++14")
        .compile("arith_final_tagless");
}
