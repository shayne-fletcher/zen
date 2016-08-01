{
open Mini_ml_parser
open Lexing

let loc lexbuf = 
  let open Lexing in
  (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let advance_line lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- 
    { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

let advance_line lexbuf = 
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- 
    { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let digit=['0'-'9']
let alpha=['a'-'z' 'A'-'Z']
let alnum = alpha|digit
let us = '_'
let quote = '\''
let double_quote = '\"'
let identifier = (us|alpha) (us|alnum|quote)*

(*

http://stackoverflow.com/questions/2293780/how-to-detect-a-floating-point-number-using-a-regular-expression

[This is the answer from the professor]

Define:

N = [1-9]
bD = 0 | N
E = [eE] [+-]? D+
L = 0 | ( N D* )

Then floating point numbers can be matched with:

( ( L . D* | . D+ ) E? ) | ( L E )
*)

let N = ['1'-'9']
let D = '0'| N
let E = ['e' 'E'] ['+' '-']? D+
let L = '0' | (N D*)
let float_=( (L '.' D* | '.' D+) E? ) | (L E)
let newline = '\n'
let date_= (D D D D '-' D D '-' D D)
let commentline='%' [^ '\n']* '\n'

rule token = parse
  | "(*"                                      { comments 0 lexbuf }
  | ['\n']                    { advance_line lexbuf; token lexbuf }
  | [' ' '\t' '\r' ]                               { token lexbuf }
  | commentline                                    { token lexbuf }
  | ";"                                  { SEMICOLON (loc lexbuf) }
  | "->"                                     { ARROW (loc lexbuf) }
  | ','                                     {  COMMA (loc lexbuf) }
  | '+'                                       { PLUS (loc lexbuf) }
  | '-'                                      { MINUS (loc lexbuf) }
  | '*'                                      { TIMES (loc lexbuf) }
  | '/'                                     { DIVIDE (loc lexbuf) }
  | '('                                     { LPAREN (loc lexbuf) }
  | ')'                                     { RPAREN (loc lexbuf) }
  | "="                                         { EQ (loc lexbuf) }
  | "<>"                                        { NE (loc lexbuf) }
  | '<'                                         { LT (loc lexbuf) }
  | "<="                                        { LE (loc lexbuf) }
  | '>'                                         { GT (loc lexbuf) }
  | ">="                                        { GE (loc lexbuf) }
  | "()"                                      { UNIT (loc lexbuf) }
  | "fun"                                      { FUN (loc lexbuf) }
  | "let"                                      { LET (loc lexbuf) }
  | "rec"                                      { REC (loc lexbuf) }
  | "in"                                        { IN (loc lexbuf) }
  | "and"                                      { AND (loc lexbuf) }
  | "or"                                        { OR (loc lexbuf) }
  | "not"                                      { NOT (loc lexbuf) }
  | "then"                                    { THEN (loc lexbuf) }
  | "else"                                    { ELSE (loc lexbuf) }
  | "if"                                        { IF (loc lexbuf) }
  | "true"                                    { TRUE (loc lexbuf) }
  | "false"                                  { FALSE (loc lexbuf) }
  | "exp"                                      { EXP (loc lexbuf) }
  | "log"                                      { LOG (loc lexbuf) }
  | "sqrt"                                    { SQRT (loc lexbuf) }
  | "len"                                      { LEN (loc lexbuf) }
  | "hd"                                        { HD (loc lexbuf) }
  | "tl"                                        { TL (loc lexbuf) }
  | digit+ as i               { INT (int_of_string i, loc lexbuf) }
  | float_ as f           { FLOAT (float_of_string f, loc lexbuf) }
  | identifier as s                         { VAR (s, loc lexbuf) }
  | eof                                        { EOI (loc lexbuf) }
  | _ as c
     { raise (Mini_ml_types.Unrecognized_token (String.make 1 c)) }
and comments level = parse
  | "*)" 
      {if level=0 then token lexbuf else comments (level-1) lexbuf}
  | ['\n']         { advance_line lexbuf; (comments level lexbuf) }
  | "(*"                              { comments (level+1) lexbuf }
  | _                                     { comments level lexbuf }
  | eof                  { raise (Mini_ml_types.Unclosed_comment) }
