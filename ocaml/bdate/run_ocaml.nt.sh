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

echo Generating documentation...
mkdir -p doc
ocamldoc -intro intro -d doc -html -colorize-code -stars -sort \
  bdate_sig.mli bdate.mli bdate.ml
