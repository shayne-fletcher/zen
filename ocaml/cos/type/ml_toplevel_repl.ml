let eval e = e

let main = 
  Repl.repl
    eval
    Ml_parser.toplevel_phrase
    Ml_lexer.token
    Ml_print_ast.string_of_toplevel_phrase
