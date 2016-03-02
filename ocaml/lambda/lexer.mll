{
  open Parser
}

let alpha=['a'-'z' 'A'-'Z']

rule token = parse
    [' ' '\t' '\n']         (* skip blanks *) { token lexbuf }
  | '('                                            { Tlparen }
  | ')'                                            { Trparen }
  | '.'                                               { Tdot }
  | ((alpha)(alpha)*) as s                         { Tident s}
  | eof                                               { Teof }
