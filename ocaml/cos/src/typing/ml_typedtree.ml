open Ml_misc
open Ml_asttypes
open Ml_types

type partial = | Partial | Total

type pattern = {
  pat_desc : pattern_desc;
  pat_loc : Ml_location.t;
}

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
      Ml_longident.t loc * constructor_description * pattern list
        (** C                []
            C P              [P]
            C (P1, ..., Pn)  [P1; ...; Pn]
          *)
  | Tpat_or of pattern * pattern
        (** P1 | P2 *)

and expression =
  { exp_desc : expression_desc;
    exp_loc : Ml_location.t;
  }

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
      Ml_longident.t loc * constructor_description * expression list
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
         *)
  | T_expifthenelse of expression * expression * expression

and case = 
  {
    c_lhs : pattern;
    c_rhs : expression;
  }

and structure = 
  {
    str_desc : structure_item_desc;
    str_loc : Ml_location.t;
  }

and structure_item_desc =
  | Tstr_eval of expression
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description

and value_binding = 
  {
    vb_pat : pattern;
    vb_expr : expression;
    vb_loc : Ml_location.t;
  }

let iter_pattern_desc f = function 
  | Tpat_alias (p, _, _) -> f p
  | Tpat_tuple ps -> List.iter f ps
  | Tpat_construct (_, _, ps) -> List.iter f ps
  | Tpat_or (p1, p2) -> f p1; f p2
  | Tpat_any | Tpat_var _ | Tpat_constant _ -> ()

let map_pattern_desc f d = match d with
  | Tpat_alias (p, id, loc) -> Tpat_alias (f p, id, loc)
  | Tpat_tuple ps -> Tpat_tuple (List.map f ps)
  | Tpat_construct (lid, c, ps) -> Tpat_construct (lid, c, List.map f ps)
  | Tpat_or (p1, p2) -> Tpat_or (f p1, f p2)
  | Tpat_any | Tpat_var _ | Tpat_constant _ -> d

(*List the identifiers bound by a pattern or a let*)

let idents = ref ([] : (Ml_ident.t * string loc) list)

let rec bound_idents pat = match pat.pat_desc with
  | Tpat_var (id, s) -> idents := (id, s) :: !idents
  | Tpat_alias (p, id, s) ->
      bound_idents p; idents := (id, s) :: !idents
  | Tpat_or (p1, _) ->
    (*Invariant : both arguments bind the same variables*)
    bound_idents p1
  | d -> iter_pattern_desc bound_idents d

let pat_bound_idents pat =
  idents := [];
  bound_idents pat;
  let res = !idents in
  idents := [];
  List.map fst res

let rev_let_bound_idents_with_loc bindings =
  idents := [];
  List.iter (fun vb -> bound_idents vb.vb_pat) bindings;
  let res = !idents in idents := []; res

let let_bound_idents_with_loc pat_expr_list =
  List.rev (rev_let_bound_idents_with_loc pat_expr_list)

let rev_let_bound_idents pat = List.map fst (rev_let_bound_idents_with_loc pat)
let let_bound_idents pat = List.map fst (let_bound_idents_with_loc pat)

let alpha_var env id = List.assoc id env

let rec alpha_pat env p = match p.pat_desc with
  | Tpat_var (id, s) -> (* note the [Not_found] case*)
    {p with pat_desc = 
    try Tpat_var (alpha_var env id, s) with
    | Not_found -> Tpat_any }
  | Tpat_alias (p1, id, s) ->
    let new_p = alpha_pat env p1 in
    begin try
      {p with pat_desc = Tpat_alias (new_p, alpha_var env id, s)}
      with
      | Not_found -> new_p
    end
  | d -> 
    {p with pat_desc = map_pattern_desc (alpha_pat env) d}

let mkloc = Ml_location.mkloc
let mknoloc = Ml_location.mknoloc

