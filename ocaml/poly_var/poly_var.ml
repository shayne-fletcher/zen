type poly_var =
   | Nil
   | W of int
   | Num of float
   | Str of string
   | Bool of bool
   | Array of (poly_var list*int*int)
   ;;

exception Poly_var_cast_error of string ;;

let is_nil = 
  function | Nil -> true | _ -> false
and as_int =
  function | W i -> i | _ -> raise (Poly_var_cast_error "int expected")
and as_float =
  function | Num f -> f | _ -> raise (Poly_var_cast_error "float expected")
and as_string =
  function | Str s -> s | _ -> raise (Poly_var_cast_error "string expected")
and as_bool =
  function | Bool b -> b | _ -> raise (Poly_var_cast_error "bool expected")
and as_array = 
  function | Array t -> t | _ -> raise(Poly_var_cast_error "array expected")
;;

(*These two functions are mutually recursive.*)

let rec poly_var_array_to_string  = 
  fun (l, rows, columns) ->
    let data = String.concat ";" (List.map string_of_poly_var l)
    in "Array (["^data^"],"^(string_of_int rows)^","^(string_of_int columns)^")"
and string_of_poly_var =
  function p ->
    match p with
    | Nil -> "Nil"
    | Bool b -> "Bool "^string_of_bool b
    | W i -> "W "^(string_of_int i)
    | Num f -> "Num "^(string_of_float f)
    | Str s -> "Str \""^s^"\""
    | Array t -> poly_var_array_to_string t
;;

(* let poly_var_of_string s =  *)
(*   let lexbuf = Lexing.from_string s in *)
(*   let result : poly_var = Parser.input Lexer.token lexbuf in *)
(*   result *)

(* (\** poly_var.ml *)

(*     A type for modeling dynamically typed expressions. *)

(* (\** poly_var lexer *\) *)
(* let lexer : char Stream.t -> Genlex.token Stream.t = make_lexer ["(";")";",";"nil"] *)

(* (\** poly_var parser *\) *)
(* let rec parse = parser *)
(*   | [< e1 = parse_top >] -> e1 *)
(* and parse_top = parser *)
(*     [< 'Int i >] -> W i *)
(*   | [< 'Float f >] -> Num f *)
(*   | [< 'String s >] -> Str s *)
(*   | [< 'Kwd "nil" >] -> Nil *)
(*   | [< 'Kwd "(" ;  e1 = parse; e2 = parse_more; 'Kwd ")">] -> Array (e1::e2, 1, (List.length (e1::e2))) *)
(* and parse_more = parser *)
(*   | [< 'Kwd "," ; e1 = parse; e2 = parse_more >] -> e1::e2 *)
(*   | [< >] -> [] *)
(*   ;; *)

(* (\* poly_var parser tests *\) *)

(* let i : poly_var = parse (lexer (Stream.of_string "1"))  *)
(* and f : poly_var = parse (lexer (Stream.of_string "1.0")) *)
(* and s : poly_var = parse (lexer (Stream.of_string "\"Hello world!\"")) *)
(* and t : poly_var = parse (lexer (Stream.of_string "(1, 2, 3, 4)"))  *)
(* and e : poly_var = parse (lexer (Stream.of_string "nil")) *)
(* in  *)
(*   Printf.printf "i = %s\n" (poly_var_to_string i) ; *)
(*   Printf.printf "f = %s\n" (poly_var_to_string f) ; *)
(*   Printf.printf "s = %s\n" (poly_var_to_string s) ; *)
(*   Printf.printf "t = %s\n" (poly_var_to_string t) ; *)
(*   Printf.printf "t = %s\n" (poly_var_to_string e) ; *)
(*   ;; *)
