module type S = sig

  type expr

  val const : int -> expr
  val var : string -> expr
  val tuple : expr list -> expr

  val tuple_match : (string * expr) list -> expr -> expr -> (string * expr) list
  val as_tuple : expr -> expr

  val string_of_expression : expr -> string

end
;;

module Expr : S = 
struct

  type expr =
  | E_var of string
  | E_const of int
  | E_tuple of expr list

  let var s = E_var s
  let const i = E_const i
  let tuple t = E_tuple t

  let as_tuple = function
    | E_tuple l as t -> t
    | _ ->  failwith "Tuple expected"

  let rec tuple_match acc x y =
    match x with
    | E_var (s) -> (s, y)::acc
    | E_tuple ((h::t)) ->
      let (E_tuple l) = as_tuple y in
      let acc = tuple_match acc (tuple t) (tuple (List.tl l)) in
      tuple_match acc h (List.hd l)
    | E_tuple [] -> acc
    | _ as unk -> failwith "Match failure"

  let rec string_of_expression = 
    let string_of_list (f:'a -> string) (l:'a list) : string = 
      "(" ^ String.concat "," (List.map f l) ^ ")"  in
    function
    | E_var (s) -> s
    | E_const (i) -> string_of_int i
    | E_tuple (l) -> string_of_list string_of_expression l

end
;;

(* (a, b, t |- (1, 2, (3, 4)))*)
let x = Expr.tuple
  [Expr.var "a"; Expr.var "b"; Expr.var "t" ] in
let y = Expr.tuple [Expr.const 1; Expr.const 2;
   Expr.tuple [Expr.const 3; Expr. const 4] ] in
let strs = List.map (fun (s, e) -> s^"="^(Expr.string_of_expression e)) (Expr.tuple_match [] x y) in
Printf.printf "%s\n" (List.fold_left (fun acc s -> acc^s^",") "" strs)
;;

(* (a, b, (c, d) |- (1, 2, (3, 4)))*)
let x = Expr.tuple
  [Expr.var "a"; Expr.var "b";
   Expr.tuple [Expr.var "c"; Expr.var "d"]] 
in
let y = Expr.tuple
  [Expr.const 1; Expr.const 2;
   Expr.tuple [Expr.const 3; Expr. const 4]] in
let strs = List.map (fun (s, e) -> s^"="^(Expr.string_of_expression e)) (Expr.tuple_match [] x y) in
Printf.printf "%s\n" (List.fold_left (fun acc s -> acc^s^",") "" strs)
;;
