{
  open Parser
}

let alpha=['a'-'z' 'A'-'Z']

rule token = parse
    ['\r' ' ' '\t' '\n']         (* skip blanks *) { token lexbuf }
  | '('                                            { Tlparen }
  | ')'                                            { Trparen }
  | '\\'                                           { Tlambda }
  | '.'                                               { Tdot }
  | ((alpha)(alpha)*) as s                         { Tident s}
  | eof                                               { Teof }
