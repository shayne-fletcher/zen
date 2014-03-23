#!/bin/bash

echo Cleaning up intermediate files...
rm *.obj *.lib *.opt *.cmx *.cmxa *.cmi

echo Building libbdate.a...
cl /nologo /EHsc /c /Fo /MD /Ic:/project/boost_1_55_0/   \
       /IC:/ocamlms64/lib \
       /DBOOST_ALL_NO_LIB=1 \
   bdate_c.cpp
lib /NOLOGO /OUT:libbdate_c.lib bdate_c.obj

echo Compiling bdate.cmxa...
ocamlopt.opt -c bdate_sig.mli bdate.mli bdate.ml
ocamlopt.opt -a -o bdate.cmxa bdate.cmx

echo Compiling bdate_test.opt...
ocamlopt.opt -c -I . bdate_test.ml
ocamlopt.opt -verbose \
  -o bdate_test.exe \
  unix.cmxa libbdate_c.lib bdate.cmxa bdate_test.cmx 

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

