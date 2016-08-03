include Ml_types

open Lexing
open Format

let eval e = e

let string_of_expression e = "<expression>"

let main = 
  Repl.repl
    eval
    Ml_parser.parse_expression
    Ml_lexer.token
    string_of_expression
