(*
 * parser_utils.ml
 *
 *     Parser utility functions.
 *
 *)

(** Test the parser. *)
let parser_test infile =
   let lexbuf  = Lexing.from_channel infile in
   let rec loop () =
      let sexpr = Parser.parse Lexer.lex lexbuf in
         match sexpr with
            | None -> ()
            | Some s ->
                 Printf.printf ";; Explicit form:\n";
                 Printf.printf "%s\n" (Sexpr.string_of_expr s);
                 Printf.printf "\n;; Scheme form:\n";
                 Printf.printf "%s\n" (Sexpr.string_of_expr2 s);
                 Printf.printf "\n";
                 loop ()
   in
      loop ()




