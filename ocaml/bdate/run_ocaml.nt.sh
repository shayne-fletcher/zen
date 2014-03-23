#!/bin/bash

echo Cleaning up intermediate files...
rm *.obj *.lib *.opt *.cmx *.cmxa *.cmi

echo Compiling target libbdate.a...
cl /nologo /EHsc /c /Fo /MD /Ic:/project/boost_1_55_0/   \
       /IC:/ocamlms64/lib \
       /DBOOST_ALL_NO_LIB=1 \
   bdate_c.cpp
lib /NOLOGO /OUT:libbdate_c.lib bdate_c.obj

echo Compiling target bdate_ocaml.obj...
ocamlopt.opt -c bdate_sig.mli bdate.mli bdate.ml bdate_test.ml
ocamlopt.opt -output-obj -o bdate_ocaml.obj unix.cmxa bdate.cmx bdate_test.cmx std_exit.cmx

#  bdet_ocaml.obj: bdate.ml bdate_test.ml
#      ocamlopt.opt -output-obj -o bdate_ocaml.obj \
#        bdate.ml bdate_test.ml unix.cmxa std_exit.cmx
#  .DEFAULT: bdet_ocaml.obj

echo Compiling target bdate_test.exe...
cl /Febdate_test.exe \
  /EHsc /MD /nologo driver.c bdate_ocaml.obj \
  libbdate_c.lib \
  c:/ocamlms64/lib/libunix.lib \
  c:/ocamlms64/lib/libasmrun.lib \
  ws2_32.lib

# bdate_test.exe : libbdate_c.lib bdate_ocaml.obj driver.c
#   cl /Febdet_test$(ARTIFACT_INFIX).exe \
#     /EHsc /MD /nologo driver.c bdate_ocaml.obj \
#     libbdate_c.lib \
#     c:/ocamlms64/lib/libasmrun.lib \
#     c:/ocamlms64/lib/libunix.lib \
#     ws2_32.lib
# .DEFAULT: bdate_test.exe
# .DEFAULT: $(CProgramCopy _, $(BIN_DIR), bdate_test)

#Build procedure for driver in C (avoiding flexlink)
#
#   cl /c /nologo /EHs /MD /I"C:/ibox-current/x86_64/mlfi/lib" \
#    /I"C:/BPCDEVTOOLS/bde/2.19.0/include" \
#     hello_stubs.cpp
#   lib /nologo /out:hello_c.lib hello_stubs.obj
#
#   mlfiopt.opt -output-obj -o hello_ocaml.obj hello.mf
#
#   cl /c /EHs /MD /nologo /I"C:/ibox-current/x86_64/mlfi/lib" driver.c
#
#   link /SUBSYSTEM:CONSOLE /OUT:hello.exe driver.obj hello_c.lib hello_ocaml.obj \
#     C:/ibox-current/x86_64/mlfi/lib/libasmrun.lib \
#     C:/BPCDEVTOOLS/bde/2.19.0/lib/v100/release_dynamic_64/bsl.lib

echo Generating documentation...
mkdir -p doc
ocamldoc -intro intro -d doc -html -colorize-code -stars -sort \
  bdate_sig.mli bdate.mli bdate.ml

