type token_t = string
type username = string
type roomname = string

type ast =
| Ast_connect of username
| Ast_nick of token_t * username
| Ast_join of token_t * roomname
| Ast_privmsg of (token_t * roomname) * string

val string_of_ast : ast -> string
val ast_of_string : string -> ast

val process_message : string -> string
