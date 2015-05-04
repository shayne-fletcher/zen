(*
 * sexpr.mli
 *
 *     S-expressions.
 *
 *)


(* Type of atomic expressions. *)
type atom =
   | Atom_unit
   | Atom_bool of bool
   | Atom_int  of int
   | Atom_id   of string


(* Type of all S-expressions. *)
type expr =
   | Expr_atom of atom
   | Expr_list of expr list


(* Convert an S-expression to a string.
   This version makes the structure of the S-expression explicit. *)
val string_of_expr : expr -> string


(* Convert an S-expression to a string.
   This version prints the S-expression like a Scheme expression. *)
val string_of_expr2 : expr -> string


