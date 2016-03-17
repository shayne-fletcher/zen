let main = 
  Repl.repl
    Lambda_with_arithmetic.eval
    Lambda_with_arithmetic_parser.main 
    Lambda_with_arithmetic_lexer.token
    Lambda_with_arithmetic.string_of_t
