rm *.opt *.cmx *.cmxa

ocamlopt -c gt.ml
ocamlopt -verbose -o graph.cmxa -a gt.cmx

ocamlopt -c test_dot.ml
ocamlopt -verbose -o test_dot.opt unix.cmxa graph.cmxa test_dot.cmx

ocamldoc -verbose -d doc -html -stars \
  -I "C:/ibox-current/x86_64/msvc-12/mlfi/lib" -sort -colorize-code \
  gt.ml
