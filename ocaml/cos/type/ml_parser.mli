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
  | T_eq
  | T_lt
  | T_fun
  | T_let
  | T_rec
  | T_in
  | T_if
  | T_then
  | T_else
  | T_fst
  | T_snd
  | T_eof
  | T_int of (string)
  | T_op of (string)
  | T_ident of (string)

val parse_pattern :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ml_ast.pattern
val parse_expression :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ml_ast.expression
