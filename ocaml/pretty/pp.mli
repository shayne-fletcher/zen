(*ML for the working programmer - Paulson
  Imperative programming in ML; A pretty printer
*)
type t
(*[t] is the type of symbolic expressions, namely blocks, strings and
  breaks*)

val block : int * t list -> t
(*[blo (i, [e1; e2; ... ; en])] creates a block containing the given
  expressiosn, and specifies that the current indentation be increased
  by [i]. This indentation will be used if the block is broken*)

val string : string -> t
(*[str s] creates an expression containing the string [s]*)

val break : int -> t
(*[brk l] creates a break of length [l]; if no line break is required
  then [l] spaces will be printed instead*)

val print : out_channel * t * int -> unit
(*[pr (oc, e, m)] prints expression [e] on [oc] with a right margin of
  [m]*)
