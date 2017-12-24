(* "The art of Prolog" - Sterling, Shaprio "GADTs and exhaustiveness :
   Looking for the impossible" - Garrigue, Le Normand
*)

(*
  natural_number (0)
  natural_number (s (X)) <- natural_number (X)
*)

type zero = Zero
type _ succ = Succ

(* e.g. The type [zero succ succ] represents the number 2. *)

(*
  plus (0, X, X) <- natural_number (X)
  plus (s (X), Y, s (Z)) <- plus (X, Y, Z)
*)

type (_, _, _) plus =
  | Plus0 : (zero, 'a, 'a) plus
  | PlusS : ('a, 'b, 'c) plus -> ('a succ, 'b, 'c succ) plus
  ;;

let trivial : (zero succ, zero, zero) plus option -> bool =
  function | None -> false

let easy : (zero, zero succ, zero) plus option -> bool =
  function | None -> false

let harder : (zero succ, zero succ, zero) plus option -> bool =
  function | None -> false

let inv_zero : type a b c d. (a, b, c) plus -> (c, d, zero) plus -> bool =
  fun p1 p2 ->
    match p1, p2 with
    | Plus0, Plus0 -> true
(* Notice that the basic exhaustiveness patterns [Plus0, PlusS] and
   [PlusS, _] can be excluded.  *)
