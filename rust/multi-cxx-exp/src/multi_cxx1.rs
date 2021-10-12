#![allow(dead_code)]

#[cxx::bridge]
mod ffi {
    extern "Rust" {
        type MultiCxx1Thing;

        fn new_thing(id: i64) -> Box<MultiCxx1Thing>;
        fn print_thing(thing: Box<MultiCxx1Thing>);
        fn print_thing_ref(thing: &Box<MultiCxx1Thing>);
    }
}

use super::thing;

#[repr(C)]
pub struct MultiCxx1Thing(pub thing::Thing);

pub fn new_thing(id: i64) -> Box<MultiCxx1Thing> {
    Box::new(MultiCxx1Thing(thing::new_thing(id)))
}

pub fn print_thing(thing: Box<MultiCxx1Thing>) -> () {
    println!("I am thing {}", (*thing).0.id)
}

pub fn print_thing_ref(thing: &Box<MultiCxx1Thing>) -> () {
    println!("I am thing {}", (*thing).0.id)
}
