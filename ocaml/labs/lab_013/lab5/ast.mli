(*
 * ast.mli
 *
 *     Abstract syntax tree.
 *
 *)

(* Type of Scheme identifiers. *)
type id = string

(* Type of Scheme expressions. *)
type expr =
   | Expr_unit
   | Expr_bool   of bool
   | Expr_int    of int
   | Expr_id     of id
   | Expr_define of id * expr
   | Expr_if     of expr * expr * expr
   | Expr_lambda of id list * expr list
   | Expr_apply  of expr * expr list

(* Convert an S-expression into an AST expression. *)
val ast_of_sexpr : Sexpr.expr -> expr

(* Convert an AST expression into a string. *)
val string_of_ast : expr -> string

(* Test the conversion from S-expressions to AST expressions 
   by reading in a file, converting all S-expressions
   in the file into the internal representation of S-expressions,
   converting each of those S-expressions into AST expressions,
   and pretty-printing them. *)
val ast_test : in_channel -> unit

