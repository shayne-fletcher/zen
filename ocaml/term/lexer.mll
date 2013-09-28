(*lexer.mll*)
{
  open Parser  (*The type token is defined in parser.mli.*)
}

let alpha=['a'-'z' 'A'-'Z']

rule token = parse
    [' ' '\t' '\n']                           (* skip blanks *) { token lexbuf }
  | '('                                                               { LPAREN }
  | ')'                                                               { RPAREN }
  | ','                                                                { COMMA }
  | ((alpha)(alpha)*) as s                                            {STRING s}
  | eof                                                                    {EOI}
