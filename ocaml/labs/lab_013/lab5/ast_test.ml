(*
 * ast_test.ml
 *     Simple test of the S-expression -> AST converter.
 *)

open Ast

let _ = 
   if Array.length Sys.argv <> 2 then
      Printf.fprintf stderr "usage: %s input_filename\n" Sys.argv.(0)
   else
      let infile = open_in Sys.argv.(1) in
         try
            ast_test infile
         with (Failure f) ->
            Printf.fprintf stderr "\nERROR: %s\n" f;
         close_in infile



