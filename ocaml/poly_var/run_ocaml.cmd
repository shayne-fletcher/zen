call "C:\Program Files\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"
ocamlopt -pp "camlp4o pa_extend.cmo" -I +camlp4 -o poly_var.exe poly_var.ml

REM ocamldoc poly_var.ml -html -all-params -colorize-code -d html
