rm -f *~ *.exe *.cmi *.cmo lexer.ml parser.mli parser.ml

ocamlc -c var.ml
ocamlc -c lambda.ml
ocamlc -c repl.ml

ocamllex lambda_lexer.mll 
ocamlyacc lambda_parser.mly
ocamlc -c lambda_parser.mli lambda_parser.ml
ocamlc -c lambda_lexer.ml
ocamlc -c lambda_repl.ml

ocamlc -o lambda.exe \
  var.cmo \
  lambda.cmo \
  lambda_parser.cmo \
  lambda_lexer.cmo \
  repl.cmo \
  lambda_repl.cmo

ocamlc -c arith.ml
ocamlc -c lambda_with_arithmetic.ml
ocamllex lambda_with_arithmetic_lexer.mll 
ocamlyacc lambda_with_arithmetic_parser.mly
ocamlc -c lambda_with_arithmetic_parser.mli lambda_with_arithmetic_parser.ml
ocamlc -c lambda_with_arithmetic_lexer.ml
ocamlc -c lambda_with_arithmetic_repl.ml

ocamlc -o lambda_with_arithmetic.exe \
  var.cmo \
  lambda.cmo \
  arith.cmo \
  lambda_with_arithmetic.cmo \
  lambda_with_arithmetic_parser.cmo \
  lambda_with_arithmetic_lexer.cmo \
  repl.cmo \
  lambda_with_arithmetic_repl.cmo
