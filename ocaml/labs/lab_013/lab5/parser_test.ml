(*
 * parser_test.ml
 *     Simple test of the parser.
 *)

open Parser_utils

let _ = 
   if Array.length Sys.argv <> 2 then
      Printf.fprintf stderr "usage: %s input_filename\n" Sys.argv.(0)
   else
      let infile = open_in Sys.argv.(1) in
         parser_test infile;
         close_in infile



