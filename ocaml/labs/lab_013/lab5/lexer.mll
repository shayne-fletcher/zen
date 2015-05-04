(*
 * lexer.mll
 *
 *     bogoscheme Lexer.
 *
 *)

{
open Parser
}

let whitespace = [' ' '\t' '\r' '\n']
let integer    = '-'? ['0' - '9']+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']
let identifier = id_chars id_chars*
let commentline= ';' [^ '\n']* '\n'

rule lex = parse
  | whitespace+                 { lex lexbuf }
  | commentline                 { lex lexbuf }
  | "("                         { TOK_LPAREN }
  | ")"                         { TOK_RPAREN }
  | "#u"                          { TOK_UNIT }
  | "#t"                     { TOK_BOOL true }
  | "#f"                    { TOK_BOOL false }
  | integer as i { TOK_INT (int_of_string i) }
  | identifier as s               { TOK_ID s }
  | eof                            { TOK_EOF }
  | _ { raise (Failure ("unrecognized token: " ^ (Lexing.lexeme lexbuf))) }

{
(* Nothing. *)
}

      
