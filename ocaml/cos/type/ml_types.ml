open Lexing

exception Unrecognized_token of string
exception Unclosed_comment

type sref = {
  loc_start : Lexing.position;
  loc_end : Lexing.position
}

type 'a loc = {
  txt : 'a;
  loc : sref;
}

let curr (lexbuf : lexbuf) : sref = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
}

let init (lexbuf : lexbuf) (fname : string) : unit =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

type rec_flag = Nonrecursive | Recursive

type unop = Unop_fst | Unop_snd
type binop = Binop_add | Binop_sub | Binop_mul | Binop_eq | Binop_less

type constant =
| Pconst_int of string

type pattern = {
  ppat_desc : pattern_desc;
  ppat_loc : sref
}
and pattern_desc = 
| Ppat_constant of constant
| Ppat_construct of string loc (*true, false*)
| Ppat_var of string loc
| Ppat_pair of (pattern * pattern)

and expression = {
  pexp_desc : expression_desc;
  pexp_loc : sref
}

and expression_desc =
| Pexp_ident of string loc
| Pexp_constant of constant
| Pexp_pair of expression * expression
| Pexp_if_then_else of expression * expression * expression
| Pexp_fun of pattern * expression
| Pexp_apply of expression * expression list
| Pexp_let of rec_flag * value_binding list * expression

and value_binding = {
  pvb_pat : pattern;
  pvb_expr : expression;
  pvb_loc : sref;
}

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; }

let none = in_file "_none_";;
