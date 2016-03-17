rm -f *~ *.exe *.cmi *.cmo lexer.ml parser.mli parser.ml

ocamllex lambda_with_arithmetic_lexer.mll 
ocamlyacc lambda_with_arithmetic_parser.mly

ocamlc -c var.ml
ocamlc -c lambda.ml
ocamlc -c arith.ml
ocamlc -c lambda_with_arithmetic.ml
ocamlc -c lambda_with_arithmetic_parser.mli lambda_with_arithmetic_parser.ml
ocamlc -c lambda_with_arithmetic_lexer.ml
ocamlc -c parse.ml
ocamlc -c lambda_with_arithmetic_repl.ml

ocamlc -o lambda_with_arithmetic.exe \
  var.cmo \
  lambda.cmo \
  arith.cmo \
  lambda_with_arithmetic.cmo \
  lambda_with_arithmetic_parser.cmo \
  lambda_with_arithmetic_lexer.cmo \
  parse.cmo \
  lambda_with_arithmetic_repl.cmo
