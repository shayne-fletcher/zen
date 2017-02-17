{
  open Lexing
  open Parser
}

let blank = [' ' '\009' '\012']
let blanks = blank*
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']
let decimal_literal = ['0'-'9']['0'-'9']*
let rest = _* as rest

rule token = parse
  | blank+                       { token lexbuf }
  | eof                                 { T_eof }
  | "[" (decimal_literal as i) "]"  { T_token i }
  | "#"                                { T_hash }
  | ": " rest                      { T_txt rest }
  | "/nick"                            { T_nick }
  | "/join"                            { T_join }
  | "/privmsg"                      { T_privmsg }
  | identchar+ { T_ident (Lexing.lexeme lexbuf) }

{
}
