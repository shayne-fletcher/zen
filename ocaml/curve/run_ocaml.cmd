::Set up C toolchain.

call "C:\program files\microsoft visual studio 10.0\vc\bin\vcvars32.bat"

::Invoke ocamlopt

ocamlopt -I c:/ocamlms/lib/site-lib/calendar str.cmxa unix.cmxa calendarLib.cmx -o curve.exe roots.mli roots.ml dates.mli dates.ml flows.mli flows.ml interpolation.mli interpolation.ml

ocamldoc -html -m A roots.mli dates.mli flows.mli interpolation.mli -d c:/project/github/zen/ocaml/curve/html
