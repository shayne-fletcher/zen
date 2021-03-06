(** Abstract syntax tree after typing *)

open Ml_asttypes (*[type rec_flag], [type 'a loc]*)
open Ml_types (*Constructor and value descriptions*)

(**{2 Types}*)

(**The type of "partiality" information indiciating partial or total
   patten matchings*)
type partial = | Partial | Total

(**The type of a pattern*)
type pattern = {
  pat_desc : pattern_desc;
  pat_loc : Ml_location.t;
}

(**The type of a pattern description*)
and pattern_desc =
  | Tpat_any
        (** _ *)
  | Tpat_var of Ml_ident.t * string loc
        (** x *)
  | Tpat_alias of pattern * Ml_ident.t * string loc
        (** P as a*)
  | Tpat_constant of constant
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Tpat_tuple of pattern list
        (** (P1, ..., Pn)

             Invariant : n >= 2
        *)
  | Tpat_construct of
      Ml_longident.t loc * Ml_types.constructor_description * pattern list
        (** C                []
            C P              [P]
            C (P1, ..., Pn)  [P1; ...; Pn]
          *)
  | Tpat_or of pattern * pattern
        (** P1 | P2 *)

(**The type of an expression*)
and expression =
  { exp_desc : expression_desc;
    exp_loc : Ml_location.t;
  }

(**The type of an expression description*)
and expression_desc =
  | Texp_ident of Ml_path.t * Ml_longident.t loc * Ml_types.value_description
        (** x
            M.x
         *)
  | Texp_constant of constant
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Texp_let of rec_flag * value_binding list * expression
        (** let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
            let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Texp_function of case list * partial
        (** [Pexp_fun] and [Pexp_function] both translate to [Texp_function].
            See {!Parsetree} for more details.

            partial =
             [Partial] if the pattern match is partial
              [Total] otherwise.
         *)
  | Texp_apply of expression * expression list
        (** E0 E1 ... En

            For example:
            let f x y = x + y in
            f 3

            The resulting typedtree for the application is:

            Texp_apply (Texp_ident "f/1490",
                        [Texp_constant Const_int 3])
         *)
  | Texp_match of expression * case list * case list * partial
        (** match E0 with
            | P1 -> E1
            | P2 -> E2
            | exception P3 -> E3

            [Texp_match (E0, [(P1, E1); (P2, E2)], [(P3, E3)], _)]
         *)
  | Texp_tuple of expression list
        (** (E1, ..., EN) *)
  | Texp_construct of
      Ml_longident.t loc * Ml_types.constructor_description * expression list
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
         *)
  | T_expifthenelse of expression * expression * expression

(**The type of a case*)
and case = 
  {
    c_lhs : pattern;
    c_rhs : expression;
  }

(**The type of a structure*)
and structure = 
  {
    str_desc : structure_item_desc;
    str_loc : Ml_location.t;
  }

(**The type of a structure item*)
and structure_item_desc =
  | Tstr_eval of expression
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of Ml_types.value_description

(**The type of a value binding*)
and value_binding = 
  {
    vb_pat : pattern;
    vb_expr : expression;
    vb_loc : Ml_location.t;
  }

(** {2 Functions over the AST} *)

(**An alisas for [Ml_location.mknoloc]*)
val mknoloc : 'a -> 'a Ml_asttypes.loc

(**An alias for [Ml_location.loc]*)
val mkloc : 'a -> Ml_location.t -> 'a Ml_asttypes.loc

(**[iter_pattern_desc f p] iterates [f] over [p]*)
val iter_pattern_desc : (pattern -> unit) -> pattern_desc -> unit

(**[map_pattern_desc f p] produces a new pattern description by
   mapping [f] over [p]*)
val map_pattern_desc : (pattern -> pattern) -> pattern_desc -> pattern_desc

(**[let_bound_idents vbs] produces a list of the identifiers in the
   bound in a let (value binding list) [vbs]*)
val let_bound_idents : value_binding list -> Ml_ident.t list

(**[rev_let_bound_idents vbs] as above but in reverse order*)
val rev_let_bound_idents : value_binding list -> Ml_ident.t list

(**Like [let_bind_idents_with_loc] but with locations*)
val let_bound_idents_with_loc : 
  value_binding list -> (Ml_ident.t * string loc) list

(**[pat_bound_idents p] produces a list of the identifiers in the
   bound in the pattern [p]*)
val pat_bound_idents : pattern -> Ml_ident.t list

(** [alpha_pat env p] produces a new pattern from [p] by substitution
    of identifiers in [env]*)
val alpha_pat : (Ml_ident.t * Ml_ident.t) list -> pattern -> pattern
