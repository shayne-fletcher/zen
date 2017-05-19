open Ml_types

val generic_level : int

val newgenty: type_desc -> type_expr
  (*Create a generic type*)

val newty2 : int -> type_desc -> type_expr
  (*Create a type*)

val newgenvar: ?name:string -> unit -> type_expr
  (* Return a fresh generic variable *)
