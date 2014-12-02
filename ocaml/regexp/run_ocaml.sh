rm *.obj *.cmx *.cmi *~ parser.ml parser.mli lexer.ml

# ocamllex lexer.mll
# ocamlyacc parser.mly

# ocamlopt.opt -c syntax.ml
# ocamlopt.opt -c parser.mli
# ocamlopt.opt -c lexer.ml
# ocamlopt.opt -c parser.ml
# ocamlopt.opt -c recognizer_sig.mli recognizer.mli recognizer.ml
# ocamlopt.opt -c analyzer_sig.mli analyzer.mli analyzer.ml
# ocamlopt.opt -c lexical_analysis_sig.mli lexical_analysis.mli lexical_analysis.ml
# ocamlopt.opt -c regexp.ml
# ocamlopt.opt -c test.ml

# ocamlopt.opt -o regexp.exe syntax.cmx lexer.cmx parser.cmx recognizer.cmx analyzer.cmx lexical_analysis.cmx regexp.cmx test.cmx


