ocamllex lexer.mll
ocamlyacc parser.mly

ocamlc -c term_types.mli
ocamlc -c term_types.ml
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c term.ml
ocamlc -o term.exe term_types.cmo lexer.cmo parser.cmo term.cmo
