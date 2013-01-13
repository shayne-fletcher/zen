#!/bin/bash

echo "Welcome!"
echo "--- Setting up build toolchain..."

. "$HOME/.msenv32" #Generate this via c:/project/make_msenv.cmd

echo "   * cl -> `which cl`..."
echo "   * link -> `which link`..."
echo "   * flexlink -> `which flexlink`..."
echo "   * python -> `which python`..."
echo "   * ocaml -> `which ocaml`..."
echo "   * ocamlc -> `which ocamlc`..."
echo "   * ocamlopt -> `which ocamlopt`..."
echo "   * ocamlopt.opt -> `which ocamlopt.opt`..."
echo "   * cmake -> `which cmake`..."
echo "   * nmake -> `which nmake`..."
echo "   * omake -> `which omake`..."
echo "   * git -> `which git`"
echo "--- All done. Happy hacking!"

#echo "   * ocamlfind ->`which ocamlfind`..."
