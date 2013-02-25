ocamlc -c echo_c.c
ocamlc -cclib "-custom-crt msvcrtd.lib -implib -link /def:echo.def" -output-obj -I %OCAMLLIB% -o echo-d.dll echo_caml.ml echo_c.obj 
