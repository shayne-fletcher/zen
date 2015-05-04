ocamllex lexer.mll
ocamlyacc parser.mly

ocamlc -c sexpr.mli sexpr.ml
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c lexer_utils.mli lexer_utils.ml
ocamlc -c lexer_test.ml
ocamlc -o lexer_test.exe sexpr.cmo lexer.cmo parser.cmo  lexer_utils.cmo lexer_test.cmo

ocamlc -c parser_utils.mli parser_utils.ml
ocamlc -c parser_test.ml

ocamlc -o parser_test.exe sexpr.cmo lexer.cmo parser.cmo parser_utils.cmo parser_test.cmo

ocamlc -c ast.mli ast.ml
ocamlc -c ast_test.ml
ocamlc -o ast_test.exe sexpr.cmo lexer.cmo parser.cmo ast.cmo ast_test.cmo
