(* A type for expressions. *)

type ('s) expr = 
  | X (* variable *)
  | Const of 's (* constant *)
  | BinOp of ((('s * 's) -> 's) * ('s)expr * ('s)expr)  (* '+','-',etc. *)
  ;;

(* Evaluate by destructuring. *)

let rec eval : 's -> ('s) expr -> 's = 
  fun t e -> 
    match e with
    | X -> t
    | Const a -> a
    | BinOp (f,l, r) -> f ((eval t l),(eval t r))
   ;;

(* Overload arithmetic operators for expressions. *)

let (+) : ('s) expr -> ('s) expr -> ('s) expr = 
  fun x y -> BinOp ((fun (a, b) -> (+) a b), x, y) 
  ;;
let (-) : ('s) expr -> ('s) expr -> ('s) expr = 
  fun x y -> BinOp ((fun (a, b) -> (-) a b), x, y) 
  ;;
let (/) : ('s) expr -> ('s) expr -> ('s) expr = 
  fun x y -> BinOp ((fun (a, b) -> (/) a b), x, y) 
  ;;
let ( * ) : ('s) expr -> ('s) expr -> ('s) expr = 
  fun x y -> BinOp ((fun (a, b) -> ( * ) a b), x, y) 
  ;;

(* Some evaluations. *)

let x : (int) expr =  X ;;
let one : (int) expr = Const 1 ;;
let two : (int) expr = Const 2 ;;
let _ =
  Printf.printf "x + 2 (at x = 3) = %d\n" (eval 3 (x + two));
  Printf.printf "x - 2 (at x = 3) = %d\n" (eval 3 (x - two));
  Printf.printf "x * 2 (at x = 3) = %d\n" (eval 3 (x * two));
  Printf.printf "x / 2 (at x = 3) = %d\n" (eval 3 (x / two));
  ;;
