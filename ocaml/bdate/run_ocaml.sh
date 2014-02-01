#!/bin/bash

echo Cleaning up intermediate files...
rm *.o *.a *.opt *.cmx *.cmxa *.cmi

echo Building libbdate.a...
g++ -c -I/home/fletch/project/boost_1_55_0   \
       -I/home/fletch/.opam/4.00.1/lib/ocaml \
  bdate_c.cpp
ar rvs libbdate_c.a bdate_c.o

echo Compiling bdate.cmxa...
ocamlopt.opt -c bdate_sig.mli bdate.mli bdate.ml
ocamlopt.opt -a -o bdate.cmxa bdate.cmx

echo Compiling bdate_test.opt...
ocamlopt.opt -c -I . bdate_test.ml
#Take care to get the ordering right here
ocamlopt.opt -verbose -cclib -lstdc++ \
  -o bdate_test \
  unix.cmxa libbdate_c.a bdate.cmxa bdate_test.cmx 

echo Generating documentation...
mkdir -p doc
ocamldoc -intro intro -d doc -html -colorize-code -stars -sort \
  bdate_sig.mli bdate.mli bdate.ml
