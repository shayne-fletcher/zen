#rm *.obj *.cmx *.cmi *~ parser.ml parser.mli lexer.ml

ocamllex lexer.mll
ocamlyacc parser.mly

ocamlopt.opt -c syntax.ml
ocamlopt.opt -c parser.mli
ocamlopt.opt -c lexer.ml
ocamlopt.opt -c parser.ml
ocamlopt.opt -c regexp.ml
ocamlopt.opt -c test.ml
ocamlopt.opt -o regexp.exe syntax.cmx lexer.cmx parser.cmx regexp.cmx test.cmx

