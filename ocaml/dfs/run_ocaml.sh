#!/bin/bash

rm *.cmi *.cmx *~ *.o
ocamlopt.opt -verbose -o dfs dfs.ml
