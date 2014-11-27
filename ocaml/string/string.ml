include String_types

let parse_string s =
  let parse_buf lexbuf =
    try Parser.main Lexer.token lexbuf
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
                "file \"\", line %d, character %d\nError : Syntax error \"%s\"" line cnum tok))
      end
  in parse_buf (Lexing.from_string s)

let print_string (s:string_t) =
 match s with
 | Const_string str -> Printf.printf "%s" str

let () = print_string (parse_string "Hello world")
