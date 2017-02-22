type ast =
| Ast_connect of string
| Ast_nick of string * string
| Ast_join of string * string
| Ast_privmsg of (string * string) * string

