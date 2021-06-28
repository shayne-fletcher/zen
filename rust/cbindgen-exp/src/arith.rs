use libc::c_char;

#[derive(Debug, Clone)]
#[repr(C)]
pub enum Term<'alloc> {
    Lit(i64),
    Neg(&'alloc Term<'alloc>),
    Add(&'alloc Term<'alloc>, &'alloc Term<'alloc>),
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

    use super::Term;
    use std::str::FromStr;

    type ParseResult<'alloc, 'text> = IResult<&'text [u8], Term<'alloc>>;

    fn lit<'alloc, 'text>(_: &'alloc bumpalo::Bump, i: &'text [u8]) -> ParseResult<'alloc, 'text> {
        map_res(delimited(space, digit, space), |x: &[u8]| {
            FromStr::from_str(std::str::from_utf8(x).unwrap()).map(Term::Lit)
        })(i)
    }

    fn neg<'alloc, 'text>(
        alloc: &'alloc bumpalo::Bump,
        i: &'text [u8],
    ) -> ParseResult<'alloc, 'text> {
        map(
            delimited(space, preceded(char('-'), |x| term(alloc, x)), space),
            |t| Term::Neg(alloc.alloc(t)),
        )(i)
    }

    fn par<'alloc, 'text>(
        alloc: &'alloc bumpalo::Bump,
        i: &'text [u8],
    ) -> ParseResult<'alloc, 'text> {
        delimited(
            space,
            delimited(tag("("), |x| expr(alloc, x), tag(")")),
            space,
        )(i)
    }

    fn term<'alloc, 'text>(
        alloc: &'alloc bumpalo::Bump,
        i: &'text [u8],
    ) -> ParseResult<'alloc, 'text> {
        alt((|x| lit(alloc, x), |x| neg(alloc, x), |x| par(alloc, x)))(i)
    }

    pub fn expr<'alloc, 'text>(
        alloc: &'alloc bumpalo::Bump,
        i: &'text [u8],
    ) -> ParseResult<'alloc, 'text> {
        let (i, init) = term(alloc, i)?;
        fold_many0(
            pair(char('+'), |x| term(alloc, x)),
            init,
            |acc, (_, val)| Term::Add(alloc.alloc(acc), alloc.alloc(val)),
        )(i)
    }
}

pub fn parse<'alloc, 'text>(
    alloc: &'alloc bumpalo::Bump,
    s: &'text [u8],
) -> Result<Term<'alloc>, String> {
    match parse::expr(alloc, s) {
        Ok((s, rep)) if s.is_empty() => Ok(rep),
        Ok((_s, _)) => Err(format!("Syntax error")),
        Err(e) => Err(format!("{}", e)),
    }
}

#[no_mangle]
pub unsafe extern "C" fn parse_ffi_alloc() -> *const std::ffi::c_void {
    Box::into_raw(Box::new(bumpalo::Bump::new())) as *const std::ffi::c_void
}

#[no_mangle]
pub unsafe extern "C" fn parse_ffi_free_alloc(alloc: *const std::ffi::c_void) {
    let _ = Box::from_raw(alloc as *mut bumpalo::Bump);
}

#[no_mangle]
pub unsafe extern "C" fn parse_ffi<'alloc, 'text>(
    alloc: *const std::ffi::c_void,
    s: *const c_char,
) -> *mut Term<'alloc> {
    match std::panic::catch_unwind(|| {
        let alloc: &'alloc bumpalo::Bump = &*(alloc as *const bumpalo::Bump);
        let text: &'text [u8] = std::ffi::CStr::from_ptr(s).to_bytes();
        match parse(alloc, text) {
            Ok(t) => alloc.alloc(t) as *mut Term<'alloc>,
            Err(_) => std::ptr::null_mut(),
        }
    }) {
        Ok(ptr) => ptr,
        Err(_) => {
            eprintln!("Error: panic in 'parse_ffi'");
            std::ptr::null_mut()
        }
    }
}
