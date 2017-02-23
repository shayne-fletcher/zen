del *.cmi *.cmo request_parser.mli request_parser.ml request_lexer.ml

ocamlc -c ast.mli
ocamlyacc request_parser.mly
ocamllex request_lexer.mll

ocamlc -thread -o server.exe unix.cma threads.cma ^
  request_parser.mli request_parser.ml request_lexer.ml server.ml

ocamlc -thread -o client.exe str.cma unix.cma threads.cma client.ml

::ocamlmktop -thread -o stop.exe unix.cma threads.cma^
::  request_parser.mli request_parser.ml request_lexer.ml ^
::  server.ml

