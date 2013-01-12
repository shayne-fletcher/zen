#!/bin/bash

echo "Welcome!"
echo "--- Setting up C/C++ build toolchain..."

. "$HOME/.msenv32" #Generate this via c:/project/make_msenv.cmd

echo "   * cl -> `which cl`..."
echo "   * link -> `which link`..."
echo "   * flexlink -> `which flexlink`..."
echo "   * python -> `which python`..."
echo "   * ocaml -> `which ocaml`"...
echo "   * ocamlc -> `which ocamlc`"...
echo "   * ocamlopt -> `which ocamlopt`"...
echo "   * ocamlopt.opt -> `which ocamlopt.opt`"...
echo "--- All done. Happy hacking!"
