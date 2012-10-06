type poly_var =
   | Nil
   | W of int
   | Num of float
   | Str of string
   | Bool of bool
   | Array of (poly_var list*int*int)

exception Poly_var_cast_error of string

val as_int : poly_var -> int
val as_float : poly_var -> float
val as_string : poly_var -> string
val as_bool : poly_var -> bool 
val as_array : poly_var -> (poly_var list*int*int)

val string_of_poly_var : poly_var -> string
(* val poly_var_of_string : string -> poly_var *)
