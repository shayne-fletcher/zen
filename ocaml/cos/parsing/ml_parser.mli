type token =
  | T_true
  | T_false
  | T_plus
  | T_minus
  | T_star
  | T_lparen
  | T_rparen
  | T_comma
  | T_arrow
  | T_underscore
  | T_eq
  | T_lt
  | T_fun
  | T_let
  | T_rec
  | T_in
  | T_if
  | T_then
  | T_else
  | T_eof
  | T_int of (string)
  | T_op of (string)
  | T_ident of (string)
  | T_uident of (string)
  | T_comment of (string * Ml_location.t)
  | T_eol

val parse_pattern :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ml_ast.pattern
val parse_expression :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ml_ast.expression
val toplevel_phrase :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ml_ast.toplevel_phrase
