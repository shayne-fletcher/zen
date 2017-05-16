ocamlc -verbose -o ocaml_syntax_highlight.exe \
  -I +compiler-libs -I +ocamldoc \
     str.cma  \
     unix.cma \
     compiler-libs/ocamlcommon.cma \
     ocamldoc/odoc_info.cma \
     odoc_ocamlhtml.ml \
     driver.ml
