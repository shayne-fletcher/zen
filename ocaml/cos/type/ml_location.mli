(**Source code locations*)

open Format

(**A type representing the span of two positions*)
type t = { 
  loc_start : Lexing.position;  (**The position where it starts*)
  loc_end : Lexing.position; (**The position past the end*)
  loc_ghost : bool (**If [true] then a "ghost" range*)
}

val none : t
(**An arbitrary value describing an empty ghost range*)

val in_file : string -> t
(**Compute an empty ghost range located in a given file*)

val init : Lexing.lexbuf -> string -> unit
(**Set the file name and line number of the [lexbuf] to be the start
   of the named file*)

val curr : Lexing.lexbuf -> t
(**Get the location of the current token from the [lexbuf]*)

val symbol_rloc : unit -> t
(**Compute the span of the left-hand-side of the matched rule in the
   program source*)

val symbol_gloc : unit -> t
(**Same as [symbol_rloc] but designates the span as a ghost range*)

val rhs_loc : int -> t
(**Compute the span of the [n]th item of the right-hand-side of the
   matched rule*)

(**A type for the association of a value with a location*)
type 'a loc = { 
  txt : 'a;
  loc : t; 
}

(**Create an ['a loc] value from a value and location*)
val mkloc : 'a -> t -> 'a loc

(**Create an ['a loc] value bound to the distinguished location called
   [none]*)
val mknoloc : 'a -> 'a loc

(**A type for error reporting*)
type error =
{
  loc : t;
  msg : string;
  sub : error list;
}

(***)
val error_of_exn : exn -> error option

(**Prints the error prefix "Error:" before yielding to the format
   string*)
val errorf_prefixed : ?loc : t -> ?sub : error list 
  -> ('a, Format.formatter, unit, error) format4 -> 'a

(**)
val error_of_printer : t ->  (formatter -> 'a -> unit) -> 'a -> error

(***)
val register_error_of_exn : (exn -> error option) -> unit

(**)
val report_error : formatter -> error -> unit

(**Hook for intercepting error reports*)
val error_reporter : (formatter -> error -> unit) ref

(**Re-raise the exception if it is unknown*)
val report_exception : formatter -> exn -> unit
