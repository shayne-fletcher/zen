type ast =
| Ast_nick of string
| Ast_join of string * string
| Ast_privmsg of (string * string) * string

val string_of_ast : ast -> string
val ast_of_string : string -> ast

val process_message : string -> string
