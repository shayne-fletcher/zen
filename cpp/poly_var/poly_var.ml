(* "It's an FVar, Jim, but not as we know it..." *)

type poly_var = 
   | Nil
   | W of int
   | Num of float
   | Str of string
   | Bool of bool
   | Array of (poly_var list*int*int)
   ;;
let poly_var_is_empty : poly_var -> bool =
   fun x -> x = Nil
   ;;

exception Poly_var_cast_error of string ;;

let poly_var_as_int : (poly_var -> int) =
   fun x ->
     match x with
       | W i -> i
       | _ -> raise (Poly_var_cast_error "not an int")
       ;;

let poly_var_as_float : (poly_var -> float) =
   fun x ->
     match x with
       | Num f -> f
       | _ -> raise (Poly_var_cast_error "not a float")
       ;;

let poly_var_as_string : (poly_var -> string) =
   fun x ->
     match x with
       | Str s -> s
       | _ -> raise (Poly_var_cast_error "not a string")
       ;;

let poly_var_as_bool : (poly_var -> bool) =
   fun x ->
     match x with
       | Bool b -> b
       | _ -> raise (Poly_var_cast_error "not a bool")
       ;;

let poly_var_as_array : poly_var -> (poly_var list * int * int) =
   fun x ->
     match x with
       | Array (l, rows, cols) -> (l, rows, cols)
       | _ -> raise (Poly_var_cast_error "not a array")
       ;;

let rec poly_var_array_to_string : poly_var list -> string =
   fun x ->
     match x with
     | [] -> ""
     | e::l -> (poly_var_to_string e) ^ " " ^ (poly_var_array_to_string l)
and
   poly_var_to_string e =
     match e with
     | Nil -> "<empty>"
     | W v -> string_of_int v
     | Num r -> string_of_float r
     | Bool b -> string_of_bool b
     | Str s -> s
     | Array (l, rows, cols) -> "(" ^ poly_var_array_to_string l ^ ")"
     ;;

exception Poly_var_domain_error of string ;;

let poly_var_sum : poly_var -> poly_var -> poly_var =
   fun e ->
     match e with
       | W i ->
         (
           fun e1 ->
              match e1 with
                | W j -> W ((+) i j)
                | _ -> raise (Poly_var_domain_error "incompatible arguments to 
poly_var_sum")
       )
       | Num x ->
         (
            fun e2 ->
              match e2 with
                | Num y -> Num ((+.) x y)
                | _ -> raise (Poly_var_domain_error "incompatible arguments to 
poly_var_sum")
         )
       | _ -> raise (Poly_var_domain_error "incompatible arguments to 
poly_var_sum")
      ;;

let poly_var_sum_range : poly_var -> poly_var -> poly_var =
   let lam : poly_var -> poly_var -> poly_var = fun acc ele -> poly_var_sum acc 
ele
   in
     fun init e ->
       match e with
       | Array (l, _, _) -> List.fold_left lam init l
       | _ -> raise (Poly_var_domain_error "incompatible argument to 
poly_var_sum_range")
       ;;

(* Test *)
let x : poly_var = W 3
and y : poly_var = Num 2.17
and s : poly_var = Str "Hello world!"
and empty = Nil
in
let v : poly_var = Array ([x;y;s], 1, 3)
in
   Printf.printf "x = %s\n" (poly_var_to_string x) ;
   Printf.printf "y = %s\n" (poly_var_to_string y) ;
   Printf.printf "s = %s\n" (poly_var_to_string s) ;
   Printf.printf "v = %s\n" (poly_var_to_string v);
   Printf.printf "3 + 4 = %s\n" (poly_var_to_string (poly_var_sum (W 3) (W 4))) 
;
   Printf.printf "3. + 4. = %s\n" (poly_var_to_string (poly_var_sum (Num 3.) 
(Num 4.))) ;
   Printf.printf "empty Nil = %b\n" (poly_var_is_empty empty) ;
   Printf.printf "empty (3 + 4) = %b\n" (poly_var_is_empty (poly_var_sum (W 3) 
(W 4))) ;
   Printf.printf "sum([3, 3, 3]) = %s\n" (poly_var_to_string 
(poly_var_sum_range (W 0) (Array ([x; x; x], 1, 3))));
   Printf.printf "sum([2.17+2.17+2.17]) = %s\n" (poly_var_to_string 
(poly_var_sum_range (Num 0.) (Array ([y; y; y], 1, 3))))

(* Parser *)
open Genlex ;;
let lexer : char Stream.t -> Genlex.token Stream.t = make_lexer 
["(";")";",";"nil"]
let rec parse = parser
  | [< e1 = parse_top >] -> e1
and parse_top = parser
  | [< 'Int i >] -> W i
  | [< 'Float f >] -> Num f
  | [< 'String s >] -> Str s
  | [< 'Kwd "nil" >] -> Nil
  | [< 'Kwd "(" ; e1 = parse; e2 = parse_more; 'Kwd ")">] -> 
        Array (e1::e2, 1, (List.length (e1::e2)))
and parse_more = parser
  | [< 'Kwd "," ; e1 = parse; e2 = parse_more >] -> e1::e2
  | [< >] -> []
  ;;

(* Test parser *)
let i : poly_var = parse (lexer (Stream.of_string "1"))
let f : poly_var = parse (lexer (Stream.of_string "1.0"))
let s : poly_var = parse (lexer (Stream.of_string "\"Hello world!\""))
let t : poly_var = parse (lexer (Stream.of_string "(1, 2, 3, 4)"))
let e : poly_var = parse (lexer (Stream.of_string "nil"))
let _ =
  Printf.printf "i = %s\n" (poly_var_to_string i) ;
  Printf.printf "f = %s\n" (poly_var_to_string f) ;
  Printf.printf "s = %s\n" (poly_var_to_string s) ;
  Printf.printf "t = %s\n" (poly_var_to_string t) ;
  Printf.printf "t = %s\n" (poly_var_to_string e) ;
  ;;
