#!/bin/bash

rm *.cmi *.cmx *~ *.o
ocamlopt.opt -verbose -o merge merge.ml
ocamldoc -d doc -html -stars -sort -colorize-code merge.ml
