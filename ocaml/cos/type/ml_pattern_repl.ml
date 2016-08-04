let eval p = p

let main = 
  Repl.repl
    eval
    Ml_parser.parse_pattern
    Ml_lexer.token
    Ml_print_ast.string_of_pattern
