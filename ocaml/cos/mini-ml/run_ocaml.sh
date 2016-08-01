#!/bin/bash

rm mini-ml* *.cmi *.cmx *.obj mini_ml_lexer.ml mini_ml_parser.ml *~ > /dev/null 2>&1

ocamllex mini_ml_lexer.mll
ocamlyacc mini_ml_parser.mly

ocamlopt -c mini_ml_types_sig.mli mini_ml_types.mli mini_ml_parser.mli mini_ml.mli
ocamlopt -c mini_ml_types.ml mini_ml_lexer.ml mini_ml_parser.ml mini_ml.ml mini_ml_repl.ml

ocamlopt -o mini-ml-full.opt \
   mini_ml_types.cmx mini_ml_lexer.cmx mini_ml_parser.cmx mini_ml.cmx mini_ml_repl.cmx

