del *.cmi *.cmo request_parser.mli request_parser.ml request_lexer.ml

ocamlc -c request_ast.mli
ocamlyacc request_parser.mly
ocamllex request_lexer.mll

::A top-level for testing the server components
ocamlmktop -thread -o stop.exe unix.cma threads.cma^
  request_parser.mli request_parser.ml request_lexer.ml ^
  server.ml

::The server itself
ocamlc -thread -o server.exe unix.cma threads.cma ^
  request_parser.mli request_parser.ml request_lexer.ml ^
  server.ml server_driver.ml

::A client
ocamlc -thread -o client.exe str.cma unix.cma threads.cma client.ml


