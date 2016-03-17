rm -f *~ *.exe *.cmi *.cmo lexer.ml parser.mli parser.ml

# ocamllex lexer.mll 
# ocamlyacc parser.mly

# ocamlc -c types.ml
# ocamlc -c parser.mli parser.ml
# ocamlc -c lexer.ml

# ocamlc -c lambda.ml

# ocamlc -o lambda.exe types.cmo parser.cmo lexer.cmo lambda.cmo
