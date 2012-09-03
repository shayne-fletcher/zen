::Set up C toolchain.

call "C:\program files\microsoft visual studio 10.0\vc\bin\vcvars32.bat"

::Invoke ocamlopt

ocamlopt ^
-I c:/ocamlms/lib/site-lib/calendar str.cmxa unix.cmxa calendarLib.cmx ^
-o curve.exe ^
roots_sig.mli roots.mli roots.ml ^
dates_sig.mli dates.mli dates.ml ^
flows_sig.mli flows.mli flows.ml ^
interpolation_sig.mli interpolation.mli interpolation.ml ^
deals_sig.mli deals.mli deals.ml ^
curves_sig.mli curves.mli curves.ml ^
curve_sig.mli curve.mli ^
main.ml

ocamldoc -html -m A ^
roots_sig.mli roots.mli ^
dates_sig.mli dates.mli ^
flows_sig.mli flows.mli ^
interpolation_sig.mli interpolation.mli ^
deals_sig.mli deals.mli ^
curves_sig.mli curves.mli ^
curve_sig.mli curve.mli ^
-d c:/project/github/zen/ocaml/curve/html
