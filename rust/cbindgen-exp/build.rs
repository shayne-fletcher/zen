extern crate cbindgen;

use std::env;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_autogen_warning(
            "/* Warning, this file is autogenerated by cbindgen. Don't modify this manually. */",
        )
        .with_pragma_once(true)
        .with_namespace("arith")
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("arith.h");
}
