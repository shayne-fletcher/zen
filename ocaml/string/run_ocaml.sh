ocamllex lexer.mll
ocamlyacc parser.mly

ocamlopt.opt -c parser.mli parser.ml
ocamlopt.opt -c lexer.ml
ocamlopt.opt -c string_test.ml

ocamlopt.opt -o string.exe string_types.cmx lexer.cmx parser.cmx string_test.cmx
