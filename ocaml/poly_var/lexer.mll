(*lexer.mll

  ocamllex poly_var lexer for use with ocamlyacc poly_var parser
  (parser.mly)

*)
{
  open Parser  (*The type token is defined in parser.mli.*)
}

let quote = '"'
let cchars = [^'"']*
let digit = ['0'-'9']

rule token = parse
    [' ' '\t' '\n']                           (* skip blanks *) { token lexbuf }
  | "Nil"                                                                { NIL }
  | "W"                                                                    { W }
  | "Num"                                                                { NUM }
  | "Str"                                                                { STR }
  | "Array"                                                            { ARRAY }
  | '('                                                               { LPAREN }
  | ')'                                                               { RPAREN }
  | '['                                                                  { LSB }
  | ']'                                                                  { RSB }
  | ';'                                                            { SEMICOLON }
  | ','                                                                { COMMA }
  | digit+ as i                                        { INT (int_of_string i) }
  |  "." digit+
  | digit+ "." digit* as f                           { REAL (float_of_string f)}
  | quote (cchars as s) quote                                      { STRING (s)}
