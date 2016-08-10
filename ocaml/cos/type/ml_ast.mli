open Ml_asttypes

type constant =
| Pconst_int of string

type pattern = {
  ppat_desc : pattern_desc;
  ppat_loc : Ml_location.t
}

and pattern_desc = 
| Ppat_any (*'_'*)
| Ppat_constant of constant
| Ppat_construct of string loc (*'()', 'true', 'false'*)
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
| Pexp_construct of string loc
| Pexp_if_then_else of expression * expression * expression
| Pexp_fun of pattern * expression
| Pexp_apply of expression * expression list
| Pexp_let of rec_flag * value_binding list * expression

and value_binding = {
  pvb_pat : pattern;
  pvb_expr : expression;
  pvb_loc : Ml_location.t;
}
