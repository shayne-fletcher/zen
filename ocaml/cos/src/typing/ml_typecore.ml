open Ml_parsetree
open Ml_typedtree

type recarg =
  | Allowed
  | Required
  | Rejected

let rec type_exp ?recarg env sexp =
  type_expect ?recarg env sexp (Ml_ctype.newvar ())

and type_expect ?in_function ?recarg env sexp ty_expected =
  let exp = type_expect_ ?in_function ?recarg env sexp ty_expected in
  exp

and type_expect_ ?in_function ?(recarg=Rejected) env sexp ty_expected =
  let loc = sexp.pexp_loc in

  match sexp.pexp_desc with
  | Pexp_ident lid ->
    let (path, desc) = Ml_typetexp.find_value env loc lid.txt in

    { exp_desc = Texp_ident (path, lid, desc);
      exp_loc = loc;
    }

let type_expression env sexp =
  let exp = type_exp env sexp in
  match sexp.pexp_desc with
  | _ -> exp

