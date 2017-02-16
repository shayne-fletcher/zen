type ast =
| Ast_nick of string
| Ast_join of string * string
| Ast_privmsg of (string * string) * string

