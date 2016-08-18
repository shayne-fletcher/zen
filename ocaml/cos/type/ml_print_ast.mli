(**String representations of abstract syntax*)

val string_of_pattern : Ml_ast.pattern -> string
(**Produce a string representation of a pattern*)

val string_of_expression : Ml_ast.expression -> string
(**Produce a string representation of a expression*)

val string_of_toplevel_phrase : Ml_ast.toplevel_phrase -> string
(**Produce a string representation of a toplevel phrase*)
