@echo off

::Set up C toolchain.

call "C:\program files\microsoft visual studio 10.0\vc\bin\vcvars32.bat"

::Invoke ocamlopt

ocamlopt -o ocaml_poly_var.exe poly_var.ml
