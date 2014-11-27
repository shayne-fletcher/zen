ocamllex lexer.mll
ocamlyacc -v parser.mly

ocamlopt.opt -c syntax.ml
ocamlopt.opt -c parser.mli
ocamlopt.opt -c lexer.ml
ocamlopt.opt -c parser.ml
ocamlopt.opt -c test.ml
ocamlopt.opt -o regexp.exe syntax.cmx lexer.cmx parser.cmx test.cmx

