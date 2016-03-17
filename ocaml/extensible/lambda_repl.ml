let main = 
  Repl.repl
    Lambda.eval
    Lambda_parser.main 
    Lambda_lexer.token
    Lambda.string_of_t
