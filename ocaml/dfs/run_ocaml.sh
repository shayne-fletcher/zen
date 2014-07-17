#!/bin/bash

rm *.cmi *.cmx *~ *.o
# ocamlopt.opt -verbose -o dfs dfs.ml
#ocamlopt.opt -verbose -o dfs_fold dfs_fold.ml
# ocamlopt.opt -verbose -o dfs_fold2 dfs_fold2.ml
#ocamlopt.opt -verbose -o flyd floyd_warshall.ml

ocamlopt.opt -verbose -o dfs_fold graph.mli graph.ml driver.ml
ocamldoc -d doc -html -stars -sort -colorize-code graph.mli
