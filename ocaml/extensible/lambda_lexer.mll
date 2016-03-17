{
  open Lambda_parser
}

let alpha=['a'-'z' 'A'-'Z']

rule token = parse
    ['\r' ' ' '\t' '\n']              { token lexbuf }
  | '('                                    { Tlparen }
  | ')'                                    { Trparen }
  | '\\'                                   { Tlambda }
  | '.'                                       { Tdot }
  | ((alpha)(alpha)*) as s                 { Tident s}
  | eof                                       { Teof }
