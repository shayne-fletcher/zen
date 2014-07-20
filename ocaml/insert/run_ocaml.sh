#!/bin/bash

rm *.cmi *.cmx *~ *.o
ocamlopt.opt -verbose -o insert insert.ml
ocamldoc -d doc -html -stars -sort -colorize-code insert.ml
