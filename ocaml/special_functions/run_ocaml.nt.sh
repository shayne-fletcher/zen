#!/bin/bash

echo Cleaning up intermediate files...
rm *.obj *.lib *.opt *.cmx *.cmxa *.cmi

echo Compiling target libspecial_functions.a...
cl /nologo /EHsc /c /Fo /MD /Ic:/project/boost_1_55_0/   \
       /IC:/ocamlms64/lib \
       /DBOOST_ALL_NO_LIB=1 \
   special_functions_c.cpp
lib /NOLOGO /OUT:libspecial_functions_c.lib special_functions_c.obj

echo Compiling target special_functions_ocaml.obj...
ocamlopt.opt -c special_functions_sig.mli special_functions.mli special_functions.ml special_functions_test.ml
ocamlopt.opt -output-obj -o special_functions_ocaml.obj unix.cmxa special_functions.cmx special_functions_test.cmx std_exit.cmx

#  special_functions_ocaml.obj: special_functions.ml special_functions_test.ml
#      ocamlopt.opt -output-obj -o special_functions_ocaml.obj \
#        special_functions.ml special_functions_test.ml unix.cmxa std_exit.cmx
#  .DEFAULT: special_functions_ocaml.obj

echo Compiling target special_functions_test.exe...
cl /Fespecial_functions_test.exe \
  /EHsc /MD /nologo driver.c special_functions_ocaml.obj \
  libspecial_functions_c.lib \
  c:/ocamlms64/lib/libunix.lib \
  c:/ocamlms64/lib/libasmrun.lib \
  ws2_32.lib

# special_functions_test.exe : libspecial_functions_c.lib special_functions_ocaml.obj driver.c
#   cl /Febdet_test$(ARTIFACT_INFIX).exe \
#     /EHsc /MD /nologo driver.c special_functions_ocaml.obj \
#     libspecial_functions_c.lib \
#     c:/ocamlms64/lib/libasmrun.lib \
#     c:/ocamlms64/lib/libunix.lib \
#     ws2_32.lib
# .DEFAULT: special_functions_test.exe
# .DEFAULT: $(CProgramCopy _, $(BIN_DIR), special_functions_test)

echo Generating documentation...
mkdir -p doc
ocamldoc -intro intro -d doc -html -colorize-code -stars -sort \
  special_functions_sig.mli special_functions.mli special_functions.ml

