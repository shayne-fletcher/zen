(**Analysis = Recognition + Values *)

(**A mini-combinator library for lexical analysis {i based on Cosineau
   and Mauny, section 8.4}*)
module type S = sig

  (**Result of an 'analyzer'. Indicates the result of attempting to
     produce a ['b] value from an ['a list]*)
  type ('a, 'b) parsed =
  | Returns of 'b * ('a list) (**Analysis succeed**)
  (**The ['b] is the value recognized; ['a list] is the input
     remaining*)
  | Analyze_fails (**Analysis failed*)

  (**An [('a, 'b) parser] is a function from an ['a list] to a value
     of [('a, 'b) parsed]*)
  type ('a, 'b) parser = 'a list -> ('a, 'b) parsed

  (**Can be raised by [recognize_or_raise]*)
  exception Parse_error

  (**Given the result of an analyzer, either extracts its payload
     (value of type ['b]) or raises a [Parse_error] exception*)
  val recognize_or_raise : ('a, 'b) parsed -> 'b

  (**An analyzer that recognizes the empty string. The first argument
     [v] is the return value to issue, it always succeeds and no input
     is ever consumed*)
  val epsilon : 'b -> ('a, 'b) parser

  (**Analyzers associated with tokens are built by means of functions
     returning values of type [option]*)
  val token : ('a -> 'b option) -> ('a, 'b option) parser

  (**'parser_of_char' is an analyzer constructor function that
     recognizes only it's argument*)
  val parser_of_char : 'a -> ('a, 'a option) parser

  (**A parser that results from the disjunction of two parsers*)
  val parser_disjunction : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser

  (**The conjunction of two analyzers builds the pair of values
     returned by each of them*)
  val parser_conjunction : ('a, 'b) parser -> ('a, 'c) parser -> ('a, ('b * 'c)) parser

  (**Re-organize results into data structures (more generally, apply a
     function to them) with this combinator*)
  val gives : ('a, 'b) parser -> ('b -> 'c) -> ('a, 'c) parser

end
