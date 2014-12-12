rm *.o *.obj *.cmx *.cmi *~ parser.ml parser.mli lexer.ml

ocamllex lexer.mll
ocamlyacc parser.mly

ocamlopt.opt -c syntax.ml
ocamlopt.opt -c parser.mli
ocamlopt.opt -c lexer.ml
ocamlopt.opt -c parser.ml
ocamlopt.opt -c recognizer_sig.mli recognizer.mli recognizer.ml
ocamlopt.opt -c c_type_sig.mli c_type.mli c_type.ml
ocamlopt.opt -c analyzer_sig.mli analyzer.mli analyzer.ml
ocamlopt.opt -c lexical_analysis_sig.mli lexical_analysis.mli lexical_analysis.ml
ocamlopt.opt -c print_utils_sig.mli print_utils.mli print_utils.ml
ocamlopt.opt -c regexp_sig.mli regexp.mli regexp.ml
ocamlopt.opt -c test.ml

ocamlopt.opt -o regexp.opt syntax.cmx lexer.cmx parser.cmx recognizer.cmx analyzer.cmx c_type.cmx lexical_analysis.cmx print_utils.cmx regexp.cmx test.cmx

PATH="`cygpath -p 'C:\ibox-current\x86_64\msvc-12\mlfi\lib\stublibs'`:$PATH"
SIGS=`ls *_sig.mli`
echo $SIGS
ocamldoc -I . -d doc -html -stars -sort $SIGS


