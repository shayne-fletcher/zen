let regexp_of_string s =
  let parse_buf lexbuf =
    try Parser.main Lexer.main lexbuf
    with 
    | Parsing.Parse_error ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        raise 
          (Failure
             (Printf.sprintf 
                "file \"<string>\", line %d, character %d\n\
Error : Syntax error \"%s\"" line cnum tok
             )
          )
      end
  in parse_buf (Lexing.from_string s)

let _ = 
  let s = "(a(a|b)\\*)?" in
  try 
    regexp_of_string s ;
    Printf.printf "%s\n" s
  with 
  | Failure msg -> print_endline msg
