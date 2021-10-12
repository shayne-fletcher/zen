fn main() {
    cxx_build::bridge("src/multi_cxx1.rs")
        .flag_if_supported("-std=c++14")
        .compile("multi_cxx1");
    cxx_build::bridge("src/multi_cxx2.rs")
        .flag_if_supported("-std=c++14")
        .compile("multi_cxx2");
}
