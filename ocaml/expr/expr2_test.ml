let string_of_expr2 =
  fun e -> Expr2.string_of_expr2 e

let expr2_of_string =
  fun s ->
    let lexbuf = Lexing.from_string s
    in Expr2_parser.main Expr2_lexer.token lexbuf

let _ = 
  Printf.printf "%s\n" (string_of_expr2 (expr2_of_string "2"));
  Printf.printf "%s\n" (string_of_expr2 (expr2_of_string "2+2"));
  Printf.printf "%s\n" (string_of_expr2 (expr2_of_string "pi/2"));
  Printf.printf "%s\n" (string_of_expr2 (expr2_of_string "-pi/2"))


