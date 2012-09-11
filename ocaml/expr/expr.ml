(* A type for expressions. *)

type  expr = 
  | Var of string(* variable *)
  | Const of int (* constant *)
  | BinOp of (((int * int) -> int) * expr * expr)  (* '+','-',etc. *)
  ;;

(* A lexer. Replace negative numbers in the input stream with a "-"
   sign followed by a positive integer.*)
let lex stream =
    let rec aux = parser
      | [< 'Genlex.Int n when n < 0; t = aux >] -> [< 'Genlex.Kwd "-" ; 'Genlex.Int (-n) ; t >]
      | [< 'h; t = aux >] -> [< 'h; t >]
      | [< >] -> [< >] in
  aux (Genlex.make_lexer ["(" ; ")" ; "+" ; "-" ; "*" ; "/"] stream)
;;

(* Three mutually recursive functions for parsing expressions. *)
let rec parse_atom = parser
  | [< 'Genlex.Ident s>] -> Var s
  | [< 'Genlex.Int n >] -> Const n
  | [< 'Genlex.Kwd "(" ; e=parse_expr ; 'Genlex.Kwd ")" >] -> e
and parse_factor = parser
    | [<e1 = parse_atom ; stream>] ->
      (parser
	  | [< 'Genlex.Kwd "*"; e2 = parse_factor >] -> BinOp ((fun (x, y) -> x*y), e1, e2)
	  | [< 'Genlex.Kwd "/"; e2 = parse_factor >] -> BinOp ((fun (x, y) -> x/y), e1, e2)
          | [< >] -> e1
      ) stream
and parse_expr = parser
    | [< e1 = parse_factor; stream>] ->
      (parser
	  | [< 'Genlex.Kwd "+" ; e2 = parse_expr >] -> BinOp ((fun (x, y) -> x + y), e1, e2)
	  | [< 'Genlex.Kwd "-" ; e2 = parse_expr >] -> BinOp ((fun (x, y) -> x - y), e1, e2)
          | [< >] -> e1) stream
;;

(* Evaluate by destructuring. *)

let rec eval : int ->  expr -> int = 
  fun t e -> 
    match e with
    | Var s -> t
    | Const a -> a
    | BinOp (f,l, r) -> f ((eval t l),(eval t r))
   ;;

(* Overload arithmetic operators for expressions. *)

let (+) :  expr ->  expr ->  expr = 
  fun x y -> BinOp ((fun (a, b) -> (+) a b), x, y) 
  ;;
let (-) :  expr ->  expr ->  expr = 
  fun x y -> BinOp ((fun (a, b) -> (-) a b), x, y) 
  ;;
let (/) :  expr ->  expr ->  expr = 
  fun x y -> BinOp ((fun (a, b) -> (/) a b), x, y) 
  ;;
let ( * ) :  expr ->  expr ->  expr = 
  fun x y -> BinOp ((fun (a, b) -> ( * ) a b), x, y) 
  ;;

let parse_eval v s =
  eval v (parse_expr (lex (Stream.of_string s)))
;;

parse_expr (lex (Stream.of_string "x")) ;;


(* Some evaluations. *)

let x : expr =  Var "x" ;;
let one : expr = Const 1 ;;
let two : expr = Const 2 ;;
let _ =

  Printf.printf "x + 2 (at x = 3) = %d\n" (eval 3 (x + two));
  Printf.printf "x - 2 (at x = 3) = %d\n" (eval 3 (x - two));
  Printf.printf "x * 2 (at x = 3) = %d\n" (eval 3 (x * two));
  Printf.printf "x / 2 (at x = 3) = %d\n" (eval 3 (x / two));

  Printf.printf "x + 2 (at x = 3) = %d\n" (parse_eval 3 "x+2");
  Printf.printf "x - 2 (at x = 3) = %d\n" (parse_eval 3 "x-2");
  Printf.printf "x * 2 (at x = 3) = %d\n" (parse_eval 3 "x*2");
  Printf.printf "x / 2 (at x = 3) = %d\n" (parse_eval 3 "x/2");
  ;;
