(**Regular expressions*)

(**A module for the production of recognizers of regular languages*)
module type S = sig

  (**The type of a compiled regular expression*)
  type t

  (**[compile] is a function producing a recognizer for the regular
     language specified by a string conforming to the syntax rules for
     regular expressions. For example, given the language L defined by
     "(a|b)*abb", [compile] will generate a recognizer for strings in
     L (the set of all strings made up from a's and b's ending in the
     sequence "abb")*)
  val compile : string -> t

  (**Check if a given string is in a particular regular language*)
  val re_match : t -> string -> bool

end
