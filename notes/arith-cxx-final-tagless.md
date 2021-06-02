# arith-cxx-tagless-final

The aim is to prove out the idea of an interpreter where the "front end" (lexical analysis) is in Rust and the "back end" (interpretation) is in C++.

- We'll parse and evaluate a simple language of arithmetic expressions;
  - The parser is implemented using [`nom`](https://github.com/Geal/nom) (a Rust parser combinator library);
- The ["tagless-final" idiom](http://okmij.org/ftp/tagless-final/index.html) is used to split the front and back ends;
- The interop between C++ and Rust is expressed using the [CXX library](https://github.com/dtolnay/cxx).

## Parser

Additive expression syntax we'll define by this grammar:
```
    expr := term ('+' term)*
    term := lit | '-' term | '(' expr ')'
    lit  := digits
```

The key idea of the program is this: Don't define an abstract syntax tree type, values of which are produced by parsing. Rather, as parsing unfolds, call functions defined by the the folllowing trait.
```rust
pub trait ExprSyn: Clone {
    fn lit(n: i64) -> Self;
    fn neg(t: Self) -> Self;
    fn add(u: Self, v: Self) -> Self;
}
```

With that understood, we implement the parser as a Rust library in the following way.
```rust
pub mod parse {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::char,
        character::complete::{digit1 as digit, space0 as space},
        combinator::{map, map_res},
        multi::fold_many0,
        sequence::{delimited, pair, preceded},
        IResult,
    };

    use super::ExprSyn;
    use std::str::FromStr;

    type ParseResult<'a, E> = IResult<&'a str, E>;

    fn lit<E: ExprSyn>(i: &str) -> ParseResult<E> {
        map_res(delimited(space, digit, space), |x| {
            FromStr::from_str(x).map(E::lit)
        })(i)
    }

    fn neg<E: ExprSyn>(i: &str) -> ParseResult<E> {
        map(delimited(space, preceded(char('-'), term), space), E::neg)(i)
    }

    fn par<E: ExprSyn>(i: &str) -> ParseResult<E> {
        delimited(space, delimited(tag("("), expr, tag(")")), space)(i)
    }

    fn term<E: ExprSyn>(i: &str) -> ParseResult<E> {
        alt((lit, neg, par))(i)
    }

    pub fn expr<E: ExprSyn>(i: &str) -> ParseResult<E> {
        let (i, init) = term(i)?;
        fold_many0(pair(char('+'), term), init, |acc, (_, val): (char, E)| {
            E::add(acc, val)
        })(i)
    }
}
```

To wire that up to C++ we need express a CXX "bridge".
```rust
use cxx::SharedPtr;

#[cxx::bridge]
pub mod ffi {
    unsafe extern "C++" {
        include!("arith_final_tagless/include/cpp_repr.hpp");
        type Cpp_repr;

        fn lit(i: i64) -> SharedPtr<Cpp_repr>;
        fn neg(e: SharedPtr<Cpp_repr>) -> SharedPtr<Cpp_repr>;
        fn add(l: SharedPtr<Cpp_repr>, r: SharedPtr<Cpp_repr>) -> SharedPtr<Cpp_repr>;
    }

    extern "Rust" {
        fn parse(s: String) -> Result<SharedPtr<Cpp_repr>>;
    }
}
```

The header file `cpp_repr.hpp` referenced by the bridge contains these prototypes.
```c++
#pragma once

#include <memory>
#include <cstdint>

struct Cpp_repr {
  int64_t expr;
};

using cpp_repr_t = std::shared_ptr<Cpp_repr>;

cpp_repr_t lit(int64_t i);
cpp_repr_t neg(cpp_repr_t t);
cpp_repr_t add(cpp_repr_t t, cpp_repr_t u);
```

The existence of that header is enough to finish off the Rust library.
```rust
#[allow(non_camel_case_types)]
pub type CppRepr_t = SharedPtr<ffi::Cpp_repr>;

impl ExprSyn for CppRepr_t {
    fn lit(i: i64) -> CppRepr_t {
        ffi::lit(i)
    }
    fn neg(t: CppRepr_t) -> CppRepr_t {
        ffi::neg(t)
    }
    fn add(t1: CppRepr_t, t2: CppRepr_t) -> CppRepr_t {
        ffi::add(t1, t2)
    }
}

pub fn parse(s: String) -> Result<CppRepr_t, String> {
    match parse::expr::<CppRepr_t>(s.as_str()) {
        Ok((_s, rep)) => Ok(rep),
        Err(e) => Err(format!("{}", e)),
    }
}
```

## Evaluator

We put the C++ part of the implementation in `cpp_repr.cpp` in a separate library.
```c++
#include <iostream>

#include "arith_final_tagless/include/cpp_repr.hpp"

cpp_repr_t lit(int64_t i) {
  return std::shared_ptr<Cpp_repr>{new Cpp_repr{i}};
}

cpp_repr_t neg(cpp_repr_t t) {
  return std::shared_ptr<Cpp_repr>{new Cpp_repr{-t->expr}};
}

cpp_repr_t add(cpp_repr_t t, cpp_repr_t u) {
  return std::shared_ptr<Cpp_repr>{new Cpp_repr{t->expr + u->expr}};
}
```

## Interpreter

The REPL in `main.cpp` brings the Rust and C++ libraries together into an executable.
```c++
#include <iostream>

#include "rust/cxx.h" // 'rust::Error'
#include "arith_final_tagless/src/arith_final_tagless.rs.h" // 'parse'

int main() {
  char const* prompt = "\n% ";
  std::cout << "Additive expression evalutator (type CTRL+D to exit)" << prompt;

  std::string line;
  while(std::getline(std::cin, line)) {
    try {
      if (auto p = parse(rust::String{line})) {
        std::cout << line << " = " << p->expr << prompt;
      }
    } catch (rust::Error const& e) {
      std::cerr << e.what() << prompt;
    }
  }

  return 0;
}
```

Source and build scripts and so on for this project [here](https://github.com/shayne-fletcher/zen/tree/master/rust/arith_final_tagless).
