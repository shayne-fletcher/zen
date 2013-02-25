cl -c /nologo /Z7 /D_DEBUG /MDd /DWIN32 /D_WINDOWS /D_DLL /W3 /Zm1000 /EHsc /GR /I %OCAMLLIB% echo_c.c

ocamlopt.opt -cclib "-custom-crt msvcrtd.lib -implib -link /def:echo.def" -output-obj -I %OCAMLLIB% -o echo-d.dll echo_caml.ml echo_c.obj 
