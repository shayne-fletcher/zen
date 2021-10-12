#![allow(dead_code)]

#[cxx::bridge]
mod ffi {
    extern "Rust" {
        type MultiCxx2Thing;

        fn another_new_thing(id: i64) -> Box<MultiCxx2Thing>;
        fn another_print_thing(thing: Box<MultiCxx2Thing>);
        fn another_print_thing_ref(thing: &Box<MultiCxx2Thing>);
    }
}

use super::thing;

#[repr(C)]
pub struct MultiCxx2Thing(pub thing::Thing);

pub fn another_new_thing(id: i64) -> Box<MultiCxx2Thing> {
    Box::new(MultiCxx2Thing(thing::another_new_thing(id)))
}

pub fn another_print_thing_ref(thing: &Box<MultiCxx2Thing>) -> () {
    println!("I am thing {}", (*thing).0.id)
}

pub fn another_print_thing(thing: Box<MultiCxx2Thing>) -> () {
    println!("I am thing {}", (*thing).0.id)
}
