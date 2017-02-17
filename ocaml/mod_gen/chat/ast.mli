type token_t = string
type username = string
type roomname = string

type ast =
| Ast_connect of username
| Ast_nick of token_t * username
| Ast_join of token_t * roomname
| Ast_privmsg of (token_t * roomname) * string

