module C = struct
  type ctype =
  | Void | Int | Float | Pointer of ctype

  type expr =
  | Intconst of int                   (* int constants *)
  | Floatconst of int                 (* float constants *)
  | Variable of string                (* variable *)
  | Apply of expr * expr list         (* function call *)
  | Assign of expr * expr             (* var = expr *)
  | Unary_op of string * expr         (* *expr, !expr, etc *)
  | Binary_op of string * expr * expr (* expr + expr, etc *)

  type statement =
  | Expr of expr                       (* expr; *)
  | If of expr * statement * statement (* if (cond) stmt; else stmt; *)
  | For of expr * expr * expr * statement 
(* for (init; cond; step) stmt; *)
  | Return of expr                     (* return expr; *)
  | Block of (string * ctype) list * statement list
(* {decls; stmts; } *)

  type term =
  | Var_decl of ctype
  | Fun_def of (string * ctype) list * ctype * statement

(*Type expressions are quite simple : there is no distinction between
  value types and definable types, and there is only one kind of
  definable types*)
  type val_type = ctype
  type def_type = ctype
  type kind = unit

end
