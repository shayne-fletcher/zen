OCAMLOPT=ocamlopt.opt
SUFFIX=.opt
SRC=btree.mli btree.ml

btree : btree.mli btree.ml
	${OCAMLOPT} -o btree${SUFFIX} ${SRC}

clean:
	rm -f *~ *.cmi *.cmx *.opt *.obj *.opt
