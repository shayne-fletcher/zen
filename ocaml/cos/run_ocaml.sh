rm *.ilk *.pdb *.cmi *.cmx *~ *.exe *.obj doc/*

ocamlopt.opt -c term.ml
ocamlopt.opt -c cos.ml
ocamlopt.opt -o cos.exe term.cmx cos.cmx 
