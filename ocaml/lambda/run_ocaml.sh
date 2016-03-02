rm -f *~ *.cmi *.cmo lexer.ml parser.mli parser.ml

ocamllex lexer.mll
ocamlyacc parser.mly

ocamlc -c string_dict.mli string_dict.ml
ocamlc -c lambda.mli lambda.ml
ocamlc -c parser.mli parser.ml
ocamlc -c lexer.ml

ocamlc -o lambda.exe string_dict.cmo lambda.cmo parser.cmo lexer.cmo 
