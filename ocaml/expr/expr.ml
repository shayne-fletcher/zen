(* A type for integer expressions. *)

type expr = 
  | X (* variable *)
  | Const of int (* constant *)
  | BinOp of (((int * int) -> int)*expr*expr)  (* '+','-',etc. *)
  ;;

(* Evaluate by destructuring. *)

let rec eval : int->expr->int = 
  fun t x -> 
    match x with
    | X -> t
    | Const a -> a
    | BinOp (f,l, r) -> f ((eval t l),(eval t r))
   ;;

(* Evalue x + 2 at x = 3. *)

let _ = 
  Printf.printf "x + 2 (at x = 3) = %d\n" 
     (eval 3 (BinOp ((fun (x, y) -> x + y), X, Const 2))) 
  ;;
