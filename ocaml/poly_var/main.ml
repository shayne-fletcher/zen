(* poly_var parse tests *)

let string_of_poly_var=Poly_var.string_of_poly_var

let poly_var_of_string s = 
  let lexbuf = Lexing.from_string s in
  Parser.input Lexer.token lexbuf

let _ =
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "Nil"));
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "W 3"));
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "Num 2.17"));
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "Str \"Hello world!\""));
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "Array ([W 3;Num 2.17; Str \"Hello world!\"],1,3)"))
