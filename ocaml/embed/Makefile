# From the thread,
# http://caml.inria.fr/mantis/print_bug_page.php?bug_id=4847

include $(shell ocamlc -where)/Makefile.config

byte:
	ocamlc -output-obj -o modcaml.obj mod.ml
	ocamlc -c modwrap.c
	ocamlc -c main.c
	$(MKEXE) -o main.exe modwrap.obj main.obj modcaml.obj $(PREFIX)/lib/libcamlrun.lib $(BYTECCLIBS)

opt:
	ocamlopt -output-obj -o modcaml.obj mod.ml
	ocamlopt -c modwrap.c
	ocamlopt -c main.c
	$(MKEXE) -o main.exe modwrap.obj main.obj modcaml.obj $(PREFIX)/lib/libasmrun.lib $(NATIVECCLIBS)

bytedll:
	ocamlc -c modwrap.c
	ocamlc -cclib "-implib -link /def:modwrap.def" -output-obj -o libmodwrap.dll modwrap.obj mod.ml
	ocamlc -c main.c
	$(MKEXE) -o main.exe main.obj libmodwrap.lib

clean:
	rm *.obj *.cmx *.cmo *.dll *.lib *.exe
