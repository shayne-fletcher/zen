use cxx::SharedPtr;

pub trait ExprSyn: Clone {
    fn lit(n: i64) -> Self;
    fn neg(t: Self) -> Self;
    fn add(u: Self, v: Self) -> Self;
}

pub mod parse {
    /*
    expr := term ('+' term)*
    term := lit | '-' term | '(' expr ')'
    lit  := digits
     */

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

impl ExprSyn for i64 {
    fn lit(i: i64) -> i64 {
        i
    }
    fn neg(t: i64) -> i64 {
        -t
    }
    fn add(t1: i64, t2: i64) -> i64 {
        t1 + t2
    }
}

#[derive(Debug, Clone)]
pub enum Term {
    Lit(i64),
    Neg(Box<Term>),
    Add(Box<Term>, Box<Term>),
}

impl ExprSyn for Term {
    fn lit(i: i64) -> Term {
        Term::Lit(i)
    }
    fn neg(t: Term) -> Term {
        Term::Neg(Box::new(t))
    }
    fn add(t1: Term, t2: Term) -> Term {
        Term::Add(Box::new(t1), Box::new(t2))
    }
}

impl ExprSyn for String {
    fn lit(i: i64) -> String {
        format!("{}", i)
    }
    fn neg(t: String) -> String {
        format!("-({})", t)
    }
    fn add(t1: String, t2: String) -> String {
        format!("({} + {})", t1, t2)
    }
}

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
