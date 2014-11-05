type token =
  | STRING of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
