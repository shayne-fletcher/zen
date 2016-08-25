rm *.ilk *.pdb *.cmi *.cmx *~ *.exe *.obj doc/* *.cmo

SRC=(
  tbl.mli tbl.ml
)

ocamlopt.opt -c tbl.mli tbl.ml
ocamldoc -d doc -html -stars -colorize-code $SRC
