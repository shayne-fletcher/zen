#!/bin/bash

rm *.cmi *.cmx *~ *.o
ocamlopt.opt -verbose -o dfs dfs.ml
ocamlopt.opt -verbose -o flyd floyd_warshall.ml
