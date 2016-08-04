let eval e = e

let main = 
  Repl.repl
    eval
    Ml_parser.parse_expression
    Ml_lexer.token
    Ml_print_ast.string_of_expression
