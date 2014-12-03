(**Lexical analysis*)

(**A small library for lexical analysis*)
module type S = sig

  (**Produce a list of characters from a string*)
  val explode : string -> char list

  (**Produce a string from a list of characters*)
  val implode : char list -> string

  (**Analyzers work like Recognizers but tackle the problem of
     constructing values during recognition. [Analyze_fails] indicates
     that the attempt at analysis failed *)
  module Analyzer : Analyzer_sig.S

  (**Recognizers work on lists; they consume part of a list and return
     either the rest of it or the value [Recognition_fails], indicating
     that the attempt at recognition failed*)
  module Recognizer : Recognizer_sig.S

  (**An implementation of the character classification functions in
     the <ctype> header of the C++ standard library*)
  module C_type : C_type_sig.S

end
