{
  open Expr2_parser
}
let digit=['0'-'9']
let alpha=['a'-'z' 'A'-'Z']
let alnum = alpha|digit
let us = '_'
let identifier = (us|alpha) (us|alnum)*

(*
http://stackoverflow.com/questions/2293780/how-to-detect-a-floating-point-number-using-a-regular-expression
[This is the answer from the professor]

Define:

N = [1-9]
D = 0 | N
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

rule token = parse
    [' ' '\t' '\n']                                             { token lexbuf }
  | digit+ as i                                        { INT (int_of_string i) }
  | float_ as f                                    { FLOAT (float_of_string f) }
  | identifier as s                                                    { VAR s }
  | '+'                                                                 { PLUS }
  | '-'                                                                { MINUS }
  | '*'                                                                { TIMES }
  | '/'                                                                  { DIV }
  | '('                                                               { LPAREN }
  | ')'                                                               { RPAREN }
  | eof                                                                  { EOI }

