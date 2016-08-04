exception Unrecognized_token of string
exception Unclosed_comment

type 'a loc = 'a Ml_location.loc = {
  txt : 'a;
  loc : Ml_location.t;
}

type rec_flag = Nonrecursive | Recursive

type unop = Unop_fst | Unop_snd
type binop = Binop_add | Binop_sub | Binop_mul | Binop_eq | Binop_less

type constant =
| Pconst_int of string

type pattern = {
  ppat_desc : pattern_desc;
  ppat_loc : Ml_location.t
}

and pattern_desc = 
| Ppat_constant of constant
| Ppat_construct of string loc (*true, false*)
| Ppat_var of string loc
| Ppat_pair of (pattern * pattern)

and expression = {
  pexp_desc : expression_desc;
  pexp_loc : Ml_location.t
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
  pvb_loc : Ml_location.t;
}

