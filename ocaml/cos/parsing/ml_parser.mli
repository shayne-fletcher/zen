type token =
  | T_arrow
  | T_bar
  | T_colon
  | T_coloncolon
  | T_comma
  | T_comment of (string * Ml_location.t)
  | T_else
  | T_eof
  | T_eol
  | T_eq
  | T_false
  | T_fun
  | T_ident of (string)
  | T_if
  | T_in
  | T_int of (string)
  | T_lbracket
  | T_let
  | T_lparen
  | T_lt
  | T_match
  | T_minus
  | T_op of (string)
  | T_plus
  | T_rbracket
  | T_rec
  | T_rparen
  | T_semi
  | T_star
  | T_then
  | T_true
  | T_uident of (string)
  | T_underscore
  | T_when
  | T_with

val parse_pattern :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ml_ast.pattern
val parse_expression :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ml_ast.expression
val toplevel_phrase :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ml_ast.toplevel_phrase
