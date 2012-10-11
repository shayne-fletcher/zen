{
  open Expr2_parser
}
let digit=['0'-'9']
let alpha=['a'-'z' 'A'-'Z']
let alnum = alpha|digit
let us = '_'
let ident = (us|alpha) (us|alnum)*

rule token = parse
    [' ' '\t' '\n']            { token lexbuf }     (* skip blanks *)
  | digit+ as i       { INT (int_of_string i) }
  | ident as s                        { VAR s }
  | '+'                                { PLUS }
  | '-'                               { MINUS }
  | '*'                               { TIMES }
  | '/'                                 { DIV }
  | '('                              { LPAREN }
  | ')'                              { RPAREN }
  | eof                                 { EOI }
