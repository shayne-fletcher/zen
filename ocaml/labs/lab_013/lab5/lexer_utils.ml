(*
 * lexer_utils.ml
 *
 *     Lexer utility functions.
 *
 *)

open Parser
open Lexer

(** Print a token. *)
let print_token token = 
   (match token with    
      | TOK_LPAREN ->
           Printf.printf "LEFT_PAREN\n"
      | TOK_RPAREN ->
           Printf.printf "RIGHT_PAREN\n"
      | TOK_UNIT ->
           Printf.printf "UNIT\n"
      | TOK_BOOL b ->
           Printf.printf "BOOL: %b\n" b
      | TOK_INT i ->
           Printf.printf "INT: %d\n" i
      | TOK_ID id ->
           Printf.printf "ID: %s\n" id
      | TOK_EOF -> 
           Printf.printf "EOF\n");
   flush stdout


(** Test the lexer. *)
let lexer_test infile =
   let lexbuf = Lexing.from_channel infile in
   let rec loop () =
      let token = lex lexbuf in
         print_token token;
         flush stdout;
         match token with
            | TOK_EOF -> ()
            | _       -> loop ()
   in
      loop ()




