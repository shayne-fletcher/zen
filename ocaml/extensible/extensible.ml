(*Basic language containing only variables*)
type var = [`Var of string]

(*Evaluation of a variable means looking in an environment for a
  binding and leaving it "as is" if there isn't one*)
let eval_var 
    (env : (string * ([> `Var of string ] as 'a)) list ) 
    (`Var s as v : var) : 'a =
  try
    List.assoc s env
  with
  | Not_found -> v

(*Extended language for lambda calculus*)
type 'a lambda = [var | `Abs of string * 'a | `App of 'a * 'a]
(*The language is defined as an open recursive type (subterms are of
  type ['a])*)

let gen_sym =
  let n = ref 0 in
  fun () -> incr n; "_" ^ string_of_int !n

(*Evaluation of lambda expressions*)
let eval_lambda eval_rec env : 'a lambda -> 'a = function
  | #var as v -> eval_var env v
  | `App (u, v) ->
    let v' = eval_rec env v in
    begin match eval_rec env u with
    | `Abs (s, body) -> eval_rec [s, v'] body
    | u' -> `App (u', v')
    end
  | `Abs (s, u) ->
    let s' = gen_sym () in
    `Abs (s', eval_rec ((s, `Var s') :: env) u)

(*Build a specific evaluator [eval1] for lambda by closing the
  recursion*)
let rec eval1 (env : (string * ('a lambda as 'a)) list) : 'a -> 'a = 
  eval_lambda eval1 env

(*Along the same lines, define the expr language (the language of
  arithmetic expressions) which adds numbers, addition and
  multiplication to the basic language*)
type 'a expr = [var | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a]

(*As for traditional variants, it comes in handy to have a [map]
  function that uniformly applies a function [f] to the sub-terms of an
  expression*)
let map_expr (f : _ -> 'a)  : 'a expr -> 'a = function
  | #var as v -> v
  | `Num _ as n -> n
  | `Add  (e1, e2) -> `Add (f e1, f e2)
  | `Mult (e1, e2) -> `Mult (f e1, f e2)

(*Evaluation of expressions*)
let eval_expr eval_rec env (e : 'a expr)  : 'a = 
  match map_expr (eval_rec env) e with
  | #var as v -> eval_var env v
  | `Add (`Num m, `Num n) -> `Num (m + n)
  | `Mult (`Num m, `Num n) -> `Num (m * n)
  | e -> e
  (*The evaluation function is quite different to lambda : first
    evaluate all sub-terms to make redexes apparent and then pattern
    match*)

(*Define an evaluator [eval2] for the closed language ['a expr as
  'a]*)
let rec eval2 (env : (string * ('a expr as 'a)) list) : 'a -> 'a= 
  eval_expr eval2 env

(*Take the union of lambda and expr to create a full evaluator*)
type 'a lexpr = [var | 'a lambda | 'a expr]

let eval_lexpr eval_rec env : 'a lexpr -> 'a = function
  | #lambda as x -> eval_lambda eval_rec env x
  | #expr as x -> eval_expr eval_rec env x

let rec eval3 env = eval_lexpr eval3 env

(* Test it out
(\x. x * x) 2 + 5
*)
let e3 = eval3 []
  (`Add (`App (`Abs ("x", `Mult (`Var "x", `Var "x")), `Num 2), `Num 5))
(*Yay! `Num 9!*)
