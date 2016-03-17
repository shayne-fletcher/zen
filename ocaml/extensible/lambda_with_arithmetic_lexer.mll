{
  open Lambda_with_arithmetic_parser
}

let alpha=['a'-'z' 'A'-'Z']
let number= ['0'-'9']['0'-'9']*

rule token = parse
    ['\r' ' ' '\t' '\n']              { token lexbuf }
  | '('                                    { Tlparen }
  | ')'                                    { Trparen }
  | '\\'                                   { Tlambda }
  | '.'                                       { Tdot }
  | '+'                                      { Tplus }
  | '*'                                      { Tstar }
  | number as num         { Tnum (int_of_string num) }
  | ((alpha)(alpha)*) as s                 { Tident s}
  | eof                                       { Teof }
