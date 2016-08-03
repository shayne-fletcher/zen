rm *.cmi *.cmo *.cmx *~ *.exe

ocamlopt.opt -c ml_types.ml
ocamllex ml_lexer.mll
ocamlyacc ml_parser.mly
ocamlopt.opt -c ml_ast_helper.ml
ocamlopt.opt -c ml_parser.mli ml_parser.ml
ocamlopt.opt -c ml_lexer.ml

ocamlopt.opt -c repl.ml

ocamlopt.opt -c ml_pattern_repl.ml
ocamlopt.opt -o ml_pattern_repl.exe \
  ml_types.cmx ml_ast_helper.cmx \
  ml_lexer.cmx ml_parser.cmx \
  repl.cmx ml_pattern_repl.cmx

ocamlopt.opt -c ml_expression_repl.ml
ocamlopt.opt -o ml_expression_repl.exe \
  ml_types.cmx ml_ast_helper.cmx \
  ml_lexer.cmx ml_parser.cmx \
  repl.cmx ml_expression_repl.cmx
