ocamlyacc rpcalc.mly
ocamlc -c rpcalc.mli
ocamllex lexer.mll

ocamlc -c lexer.ml
ocamlc -c rpcalc.ml
ocamlc -c main.ml

ocamlc -o rpcalc.exe lexer.cmo rpcalc.cmo main.cmo
del rpcalc.mli lexer.ml rpcalc.ml *.cmo *.cmi *.~
