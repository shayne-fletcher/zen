include Mini_ml_types_sig.S

val expression_list_of_string : ?file:string -> string -> expression list
val expression_of_string : ?file:string -> string -> expression
val expression_list_of_file : string -> expression list
val expression_of_file : string -> expression

val string_of_expression_list : expression list -> string
val string_of_expression : expression -> string

type value = 
  | V_unit
  | V_bool of bool
  | V_int of int
  | V_float of float
  | V_closure of ((string * value) list) * expression
  | V_tuple of value list

type environment = ((string*value) list) ref

val eval : environment -> expression -> value
val eval_exprs : value list -> environment -> expression list -> value list

val value_list_of_string : ?file:string -> environment -> string -> value list
val value_of_string : ?file:string -> environment -> string -> value
val value_list_of_file : environment -> string -> value list
val value_of_file : environment -> string -> value

val string_of_value_list : value list -> string
val string_of_value : value -> string
