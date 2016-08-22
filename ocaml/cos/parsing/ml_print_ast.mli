(**Print abstract syntax*)

(**{2 String representations}*)

val string_of_pattern : Ml_ast.pattern -> string
(**Produce a string representation of a pattern*)

val string_of_expression : Ml_ast.expression -> string
(**Produce a string representation of a expression*)

val string_of_toplevel_phrase : Ml_ast.toplevel_phrase -> string
(**Produce a string representation of a toplevel phrase*)

(**{2 Format functions}*)

val toplevel_phrase : int -> Format.formatter -> Ml_ast.toplevel_phrase -> unit
(**Print a [toplevel_phrase] on a [formatter]*)

val structure :  int -> Format.formatter -> Ml_ast.structure -> unit
(**Print a [structure] on a [formatter]*)

val structure_item :  int -> Format.formatter -> Ml_ast.structure_item -> unit
(**Print a [structure_item] on a [formatter]*)

val pattern :  int -> Format.formatter -> Ml_ast.pattern -> unit
(**Print a [pattern] on a [formatter]*)

val expression :  int -> Format.formatter -> Ml_ast.expression -> unit
(**Print a [expression] on a [formatter]*)

