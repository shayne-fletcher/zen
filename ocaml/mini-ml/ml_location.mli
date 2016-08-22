(**Source code locations*)

(**{2 Locations}*)

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

(**Create an ['a loc] value from a ['a] value and location*)
val mkloc : 'a -> t -> 'a loc

(**Create an ['a loc] value bound to the distinguished location called
   [none]*)
val mknoloc : 'a -> 'a loc

(**{2 Error reporting}*)

open Format

(**A type for error reporting*)
type error =
{
  loc : t;  (**Location to associate with the error*)
  msg : string;  (**A human readable description of the error*)
  sub : error list; (**A list of associated errors*)
}

val register_error_of_exn : (exn -> error option) -> unit
(**This module contains a mutable list of exception "handlers". Client
   modules register handlers for their own exception types by means of
   this function*)

val error_of_exn : exn -> error option
(**A function that given an exception converts it to an [error
   option]. Given argument [x : exn], the list of handlers is
   traversed; each handler is invoked on [x]. The first one that
   doesn't return [None] returns [Some err] (say) else the search goes
   on. Of course, if there is no matching handler, [None] is returned
*)

val error_of_printer : t ->  (formatter -> 'a -> unit) -> 'a -> error
(**The job of this function is to convert a location and value of type
   ['a] into an [error] by means of an auxillary function that knows how
   to make a human readable message of the ['a]*)

(**This module implements a framework for implementing uniform error
   handling/reporting across modules concerned with source code
   locations across related modules (think lexer, parser, type-checker
   etc.). Let's take the lexer for an example.

   {[
     (*lexer.mll*)
     {
        (*The cases of lexer errors*)
        type error =
          | Illegal_character of char
          | Unterminated_comment of Ml_location.t

        (*The lexer exception type*)
        exception Error of error * Ml_location.t

        (*This function takes a formatter and an instance of type
          [error] and writes a message to the formatter explaining the
          meaning. This is a "printer"*)
        let report_error (ppf : Format.formatter) : error -> unit = function
         | Illegal_character c -> 
            Format.fprintf ppf "Illegal character (%s)" (Char.escaped c)
         | Unterminated_comment _ -> 
            Format.fprintf ppf "Comment not terminated"

        (*Note that [report_error] is a function that unifies with
          the [formatter -> 'a -> unit] parameter of
          [error_of_printer]*)

        (*Register an exception handler for the lexer exception type*)
        let () =
         Ml_location.register_error_of_exn
          (function
           | Error (err, loc) ->
              Some (Ml_location.error_of_printer loc report_error err)
           | _ ->  None
          )
     }

     /*...*/
     rule token = ...
  ]}
*)

val errorf_prefixed : ?loc : t -> ?sub : error list 
  -> ('a, Format.formatter, unit, error) format4 -> 'a
(**[errorf_prefixed] computes a function. The function it computes
  provides the mean to convert an ['error_t] to an [error] by way of
  formatting operations to produce the [msg] field of the [error]
  result. The formatting operations include prefixing the [msg] field
  with the string ["Error"].

  The type of the function computed unifies with the type variable
  ['a].
*)

val error_reporter : (formatter -> error -> unit) ref
(**Hook for intercepting error reports*)

val report_error : formatter -> error -> unit
(**[report_error ppf err] uses the currently installed error reporter
   to write an error report for [err] on the formatter [ppf]*)

val report_exception : formatter -> exn -> unit
(**[report_exception ppf exn] attempts to write an error report for
   the provided [exn] on the given formatter [ppf]. The exception [exn]
   can be (re-)raised if no handler is found*)
