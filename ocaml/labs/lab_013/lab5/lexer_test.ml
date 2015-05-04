(*
 * lexer_test.ml
 *     Simple test of the lexer.
 *)

open Lexer_utils

let _ = 
   if Array.length Sys.argv <> 2 then
      Printf.fprintf stderr "usage: %s input_filename\n" Sys.argv.(0)
   else
      let infile = open_in Sys.argv.(1) in
         lexer_test infile;
         close_in infile



