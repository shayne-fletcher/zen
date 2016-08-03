let main = 
  Repl.repl
    Expression.eval
    Ml_parser.parse_expression
    Ml_lexer.token
    Expression.string_of_expression
