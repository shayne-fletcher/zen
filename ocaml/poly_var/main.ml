(* poly_var tests *)

let x : Poly_var.poly_var = Poly_var.W 3
and y : Poly_var.poly_var = Poly_var.Num 2.17
and s : Poly_var.poly_var = Poly_var.Str "Hello world!"
(* and nil = Poly_var.Nil *)
in
let v : Poly_var.poly_var = Poly_var.Array ([x;y;s], 1, 3)
in
   Printf.printf "%s\n" (Poly_var.string_of_poly_var x) ;
   Printf.printf "%s\n" (Poly_var.string_of_poly_var y) ;
   Printf.printf "%s\n" (Poly_var.string_of_poly_var s) ;
   Printf.printf "%s\n" (Poly_var.string_of_poly_var v)

let string_of_poly_var=Poly_var.string_of_poly_var

let poly_var_of_string s = 
  let lexbuf = Lexing.from_string s in
  Parser.input Lexer.token lexbuf

let _ =
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "Nil"));
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "W 1"));
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "Num 1.0"));
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "Str \"Yippe kai yay!\""));
  Printf.printf "%s\n" (string_of_poly_var (poly_var_of_string "Array ([W 1;W 2; W3],1,1)"))
