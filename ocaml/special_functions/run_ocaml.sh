#!/bin/bash

echo Cleaning up intermediate files...
rm *.o *.a *.opt *.cmx *.cmxa *.cmi

echo Building libspecial_functions.a...
g++ -c -I/home/fletch/project/boost_1_55_0   \
       -I/home/fletch/.opam/4.00.1/lib/ocaml \
  special_functions_c.cpp
ar rvs libspecial_functions_c.a special_functions_c.o

echo Compiling special_functions.cmxa...
ocamlopt.opt -c special_functions_sig.mli special_functions.mli special_functions.ml
ocamlopt.opt -a -o special_functions.cmxa special_functions.cmx

echo Compiling special_functions_test.opt...
ocamlopt.opt -c -I . special_functions_test.ml
#Take care to get the ordering right here
ocamlopt.opt -verbose -cclib -lstdc++ \
  -o special_functions_test \
  unix.cmxa libspecial_functions_c.a special_functions.cmxa special_functions_test.cmx 

echo Generating documentation...
mkdir -p doc
ocamldoc -intro intro -d doc -html -colorize-code -stars -sort \
  special_functions_sig.mli special_functions.mli special_functions.ml
