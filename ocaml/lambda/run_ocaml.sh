rm -f *~ *.exe *.cmi *.cmo lexer.ml parser.mli parser.ml

ocamllex lexer.mll
ocamlyacc parser.mly

ocamlc -c string_dict.mli string_dict.ml
ocamlc -c lambda_types.mli lambda_types.ml
ocamlc -c parser.mli parser.ml
ocamlc -c lexer.ml
ocamlc -c pp.mli pp.ml
ocamlc -c lambda.mli lambda.ml
ocamlc -c reduce.mli reduce.ml
ocamlc -c driver.ml

ocamlc -o lambda.exe string_dict.cmo lambda_types.cmo parser.cmo lexer.cmo pp.cmo lambda.cmo reduce.cmo driver.cmo
