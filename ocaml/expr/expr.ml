(* A type for integer expressions. *)

type expr = 
  | X (* variable *)
  | Const of int (* constant *)
  | BinOp of (((int * int) -> int)*expr*expr)  (* '+','-',etc. *)
  ;;

let x : expr =  X ;;
let one : expr = Const 1 ;;
let two : expr = Const 2 ;;

(* Evaluate by destructuring. *)

let rec eval : int->expr->int = 
  fun t e -> 
    match e with
    | X -> t
    | Const a -> a
    | BinOp (f,l, r) -> f ((eval t l),(eval t r))
   ;;

let add : (int*int)->int = fun (a, b) -> (+) a b ;;
let sub : (int*int)->int = fun (a, b) -> (-) a b ;;
let div : (int*int)->int = fun (a, b) -> (/) a b ;;
let mul : (int*int)->int = fun (a, b) -> ( * ) a b ;;

(* Some evaluations. *)

let _ =
  let _ = 
    let _ = 

      Printf.printf "x + 2 (at x = 3) = %d\n" (eval 3 (BinOp (add, x, two))) 
    in 
      Printf.printf "x - 2 (at x = 3) = %d\n" (eval 3 (BinOp (sub, x, two))) 
  in
      Printf.printf "x * 2 (at x = 3) = %d\n" (eval 3 (BinOp (mul, x, two))) 
in
      Printf.printf "x / 2 (at x = 3) = %d\n" (eval 3 (BinOp (div, x, two))) 
