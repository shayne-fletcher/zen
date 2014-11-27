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

let safe_proc f =
  try
    f ()
  with
  | Lexer.Error (e, s) ->
    match e with
    | Lexer.Illegal_escape s -> Printf.printf "Illegal escape : %s\n" s
    | Lexer.Eol_in_string s -> Printf.printf "End of line in : %s\n" s
    | Lexer.Unterminated_string -> Printf.printf "Unterminated string\n"

let () =
  safe_proc (fun () -> Printf.printf "%s" (parse_string "\"yippee\\n\"")) ;
  safe_proc (fun () -> Printf.printf "%s" (parse_string "\"foo\\tbar\\n\"")) ;
  safe_proc (fun () -> Printf.printf "%s" (parse_string "\"foo bar baz")) ;
  safe_proc (fun () -> Printf.printf "%s" (parse_string "\"foo \x bar baz\"")) ;
  safe_proc (fun () -> Printf.printf "%s" (parse_string "\"foo\\
  bar baz\"")) ;
  ()

