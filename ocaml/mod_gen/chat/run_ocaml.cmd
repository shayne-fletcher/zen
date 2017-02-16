del *.cmi *.cmo parser.mli parser.ml lexer.ml

ocamlc -c ast.mli
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c parser.mli parser.ml lexer.ml 
ocamlc -c room.ml
ocamlc -c protocol.mli protocol.ml

:: ocaml parser.cmo lexer.cmo protocol.cmo

:: ocamlc -thread -o parser.cmo lexer.cmo server.ml unix.cma threads.cma serv_up.ml
