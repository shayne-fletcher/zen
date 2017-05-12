ocamllex odoc_ocamlhtml.mll
ocamlc -c odoc_ocamlhtml.ml
ocamlc -c driver.ml
ocamlc -o ocaml_syntax_highlight.exe str.cma odoc_ocamlhtml.cmo driver.cmo
