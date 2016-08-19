(**Abstract syntax*)

open Ml_asttypes

(**{2 Core language}*)

(**The type of constants*)
type constant =
| Pconst_int of string (**Integer constants*)

(**The type of patterns*)
type pattern = {
  ppat_desc : pattern_desc; (**Pattern description*)
  ppat_loc : Ml_location.t (**Location in the source*)
}

(**The type of pattern descriptions*)
and pattern_desc = 
| Ppat_any (*'_'*)  (**Wildcard*)
| Ppat_constant of constant (**A constant*)
| Ppat_construct of string loc (*'()', 'true', 'false'*) (**Constructor*)
| Ppat_var of string loc (**Variable*)
| Ppat_pair of (pattern * pattern) (**Pair*)

(**The type of expressions*)
and expression = {
  pexp_desc : expression_desc; (**Expression description*)
  pexp_loc : Ml_location.t (**Location in the source*)
}

(**The type of expression descriptions*)
and expression_desc =
| Pexp_ident of string loc (**Identifiers*)
| Pexp_constant of constant (**Constants*)
| Pexp_pair of expression * expression (**Pairs*)
| Pexp_construct of string loc (**Constructors*)
| Pexp_if_then_else of expression * expression * expression (**Conditionals*)
| Pexp_fun of pattern * expression (**Functions*)
| Pexp_apply of expression * expression list (**Application*)
| Pexp_let of rec_flag * value_binding list * expression (**'let bindings'*)

(**The type of value bindings*)
and value_binding = {
  pvb_pat : pattern;  (**The pattern*)
  pvb_expr : expression;  (**The bound expression*)
  pvb_loc : Ml_location.t; (**Location in the source*)
}

(**The type of structures*)
and structure = structure_item list

(**The type of structure items*)
and structure_item = {
  pstr_desc : structure_item_desc; (**Structure item description*)
  pstr_loc : Ml_location.t (**Location in the source*)
}

(**The type of structure item descriptions*)
and structure_item_desc = 
  | Pstr_eval of expression (**An evaluation*)
  | Pstr_value of rec_flag * value_binding list (**A value binding*)

(**{2 Top level}*)

(**The type of top-level phrases*)
type toplevel_phrase =
| Ptop_def of structure (**A structure definition*)

