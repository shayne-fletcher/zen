::Set up C toolchain.

call "C:\program files\microsoft visual studio 10.0\vc\bin\vcvars32.bat"

::Invoke ocamlopt

ocamlopt -pp "camlp4o pa_extend.cmo" -I +camlp4 -o expr.exe expr.ml
ocamlopt -pp "camlp4o pa_extend.cmo" -I +camlp4 -o eval.exe eval.ml

::ocamldoc -html -m A expr.ml eval.ml -d c:/project/github/zen/ocaml/expr/html
