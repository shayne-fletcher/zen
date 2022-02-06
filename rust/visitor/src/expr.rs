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

pub fn parse(s: &str) -> Result<Term, String> {
    match parse::expr::<Term>(s) {
        Ok((_s, rep)) => Ok(rep),
        Err(e) => Err(format!("{}", e)),
    }
}

/*
pub trait Visitor<'a, T: 'a> {
    fn visit_expr(&mut self, e: &'a Term) -> T;
}

pub struct Terms;
impl<'a> Visitor<'a, Vec<&'a Term>> for Terms {
    fn visit_expr(&mut self, e: &'a Term) -> Vec<&'a Term> {
        match *e {
            Term::Lit(_) => vec![e],
            Term::Neg(ref t) => {
                let mut r = vec![e];
                r.extend(self.visit_expr(&**t));
                r
            }
            Term::Add(ref u, ref v) => {
                let mut r = vec![e];
                r.extend(self.visit_expr(&**u));
                r.extend(self.visit_expr(&**v));
                r
            }
        }
    }
}
 */

// A type which can be traversed by a visitor.
pub trait Walkable<'a> {
    fn accept(&'a self, v: &mut dyn Visitor<'a>) {
        self.recurse(v);
    }
    fn recurse(&'a self, _v: &mut dyn Visitor<'a>) {}
}

// A visitor over data structures containing terms.
pub trait Visitor<'a> {
    // Must return `self`.
    fn object(&mut self) -> &mut dyn Visitor<'a>;

    fn visit_term(&mut self, t: &'a Term) {
        t.recurse(self.object());
    }
}

impl<'a, T: Walkable<'a>> Walkable<'a> for Option<T> {
    fn recurse(&'a self, v: &mut dyn Visitor<'a>) {
        match self {
            Some(some) => some.accept(v),
            None => {}
        }
    }
}

impl<'a, A: Walkable<'a>, B: Walkable<'a>> Walkable<'a> for (A, B) {
    fn recurse(&'a self, v: &mut dyn Visitor<'a>) {
        self.0.accept(v);
        self.1.accept(v);
    }
}

impl<'a> Walkable<'a> for Term {
    fn accept(&'a self, v: &mut dyn Visitor<'a>) {
        v.visit_term(self);
    }

    fn recurse(&'a self, v: &mut dyn Visitor<'a>) {
        match *self {
            Term::Neg(ref t) => {
                v.visit_term(&**t);
            }
            Term::Add(ref s, ref t) => {
                v.visit_term(&**s);
                v.visit_term(&**t);
            }
            _ => {}
        }
    }
}
