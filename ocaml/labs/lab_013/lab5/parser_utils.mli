(*
 * parser_utils.mli
 *
 *     Parser utility functions.
 *
 *)

(* Test the parser by reading in a file, converting all S-expressions
   in the file into the internal representation of S-expressions,
   and pretty-printing them. *)
val parser_test : in_channel -> unit

