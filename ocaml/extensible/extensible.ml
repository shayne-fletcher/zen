(*Basic language containing only variables*)
type var = [`Var of string]

let string_of_var : var  -> 'a = function
  | `Var s -> s

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

module Detail = struct
  let rec strip (bs : string list) t =
    match t with 
    | `Abs (b, t) -> strip (b :: bs) t
    | _ as u -> (List.rev bs, u)
  end (*module Detail*)

let string_of_lambda string_of_rec = function
  | #var as v -> string_of_var v
  | `App (u, v) -> 
    "("^(string_of_rec u) ^ ") (" ^ (string_of_rec v)^")"
  | `Abs _ as t -> 
    match (Detail.strip [] t) with
    | (b :: bs, u)  ->
      let binder = 
        "\\" ^ b ^ (List.fold_right  (fun z b -> " " ^ z ^ b) bs ". ") in
      binder ^ (string_of_rec u)
    | ([], _) -> assert false

(*Factory functions*)
let mk_var : string -> [> `Var of string] = fun s -> `Var s
let mk_abs : string * 'b -> [> `Abs of string * 'b] = fun (s, t) -> `Abs (s, t)
let mk_app : 'a * 'b -> [> `App of 'a * 'b] = fun (u, v) -> `App (u, v)

let gen_sym =
  let n = ref 0 in
  fun () -> incr n; "_" ^ string_of_int !n

(*Evaluation of lambda expressions*)
let eval_lambda eval_rec env : 'a lambda -> 'a = function
  | #var as v -> eval_var env v
  | `App (u, v) ->
    (*Evaluate the operand*)
    let v' = eval_rec env v in
    (*Next evaluate the operator*)
    begin match eval_rec env u with
    | `Abs (s, body) -> 
      (*If it's an abstraction, evaluate the body in an environment
        where the binder [s] is bound to [v']*)
      eval_rec [s, v'] body
    | u' -> `App (u', v') (*No further reduction is possible*)
    end
  | `Abs (s, u) ->
    (*Interesting rule. To evaluate, create an environment where [s]
      is bound to a variable [`Var s']() and evaluate the body in that
      environment*)
    let s' = gen_sym () in
    `Abs (s', eval_rec ((s, `Var s') :: env) u)
    (*This has the effect of making every abstraction unique in terms
      of their binders e.g.
      (1)
        `Abs ("x", `Mult (`Var "x", `Var "x")` -> 
          `Abs ("_1", `Mult (`Var "_1", `Var "_`"))
      (2)
        `Abs ("s", (`Abs ("t", (`Mult (`Var "s", `Var "t")))))) ->
           `Abs ("_2", `Abs ("_3", `Mult (`Var "_2", `Var "_3")))
    *)

(*Build a specific evaluator [eval1] for lambda by closing the
  recursion*)
let rec eval1 (env : (string * ('a lambda as 'a)) list) : 'a -> 'a = 
  eval_lambda eval1 env

(*Along the same lines, define the expr language (the language of
  arithmetic expressions) which adds numbers, addition and
  multiplication to the basic language*)
type 'a expr = [var | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a]

let mk_num i = `Num i
let mk_add (u, v) = `Add (u, v)

let string_of_expr string_of_rec = function
  | #var as v -> 
    string_of_var v
  | `Num i -> 
    string_of_int i
  | `Add (u, v) -> 
    "(" ^ (string_of_rec u) ^ " + " ^ (string_of_rec v) ^ ")"
  | `Mult (u, v) -> 
    "(" ^ (string_of_rec u) ^ " * " ^ (string_of_rec v) ^ ")"

(*As for traditional variants, it comes in handy to have a [map]
  function that uniformly applies a function [f] to the sub-terms of an
  expression*)
let map_expr 
  (f : (
    [> `Add of 'a * 'a | `Mult of 'a * 'a | `Num of int | `Var of string ]
    as 'a) -> 'a)  : 'a expr -> 'a = function
  | #var as v -> v
  | `Num _ as n -> n
  | `Add  (e1, e2) -> `Add (f e1, f e2)
  | `Mult (e1, e2) -> `Mult (f e1, f e2)

(*Evaluation of expressions*)
let eval_expr eval_rec  
  (env : (string *
      ([> `Add of 'a * 'a | `Mult of 'a * 'a | `Num of int | `Var of string ]
       as 'a))
     list)
  (e : 'a expr)  : 'a = 
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

let rec string_of_t = function
  | #lambda as l -> string_of_lambda string_of_t l
  | #expr as e -> string_of_expr string_of_t e

let eval_lexpr eval_rec 
    (env : (string *
    ([> `Abs of string * 'a
      | `Add of 'a * 'a
      | `App of 'a * 'a
      | `Mult of 'a * 'a
      | `Num of int
      | `Var of string ]
     as 'a)) list) : 'a lexpr -> 'a = function
  | #lambda as x -> eval_lambda eval_rec env x
  | #expr as x -> eval_expr eval_rec env x

let rec eval3 (env : (string * ('a lexpr as 'a)) list) : 'a -> 'a = 
  eval_lexpr eval3 env

(* Test it out
(\x. x * x) 2 + 5
*)
let e3 = eval3 []
  (`Add (`App (`Abs ("x", `Mult (`Var "x", `Var "x")), `Num 2), `Num 5))
(*Yay! `Num 9!*)
