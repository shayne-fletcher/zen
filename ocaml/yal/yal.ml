type term =
| Var of int
| Abs of term
| App of term * term

let rec shift (c : int) (d : int) : term -> term = function
  | Var k as v when k < c -> v
  | Var k -> Var (k + d)
  | Abs t -> Abs (shift (c + 1) d t)
  | App (u, v) -> App (shift c d u, shift c d v)

let rec subst (j : int) (s : term) : term -> term = function
  | Var k when k = j -> s
  | Var _ as v -> v
  | Abs t -> Abs (subst (j + 1) (shift 0 1 s) t)
  | App (u, v) -> App (subst j s u, subst j s v)

let rec is_val : term -> bool = 
  function  | Abs _ -> true | _ -> false

let term_subst_top (s : term) (t : term) : term =
 shift 0 (-1) (subst 0 (shift 0 1 s) t)

exception No_rule_applies

let rec eval1 t = 
  match t with
  | App (Abs t12, v2) when is_val v2 -> 
    term_subst_top v2 t12
  | App (v1, t2) when is_val v1 -> 
    let t2' = eval1 t2 in  App (v1, t2')
  | App (t1, t2) -> 
    let t1' = eval1 t1 in  App (t1', t2)
  | _ -> raise No_rule_applies

let rec eval t =
  try
    let t' = eval1 t in
    eval t'
  with
  | No_rule_applies -> t

let rec big_eval t =
  match t with
  | App (u, v) ->
    let v' = big_eval v in
    begin 
      match big_eval u with
      | Abs b -> big_eval (term_subst_top v' b)
      | _ as u' -> App (u', v')
    end
  | Abs body -> Abs (big_eval body)
  | _ as v -> v

(* \x.\y. x *)
let t = Abs (Abs (Var 1))

(* \x.\y. y *)
let f = Abs (Abs (Var 0))

(* \p.\x.\y. p x y *)
let if_ = 
  Abs (Abs ( Abs (
    App (App (Var 2, Var 1), Var 0)
  )))

(*(\p.\x.\y. p x y) (\x.\y. x) *)
let res = eval (App (if_,  t))
let res1 = eval (App (if_,  t))
let res = eval (App (if_,  f))
let res2 = big_eval (App (if_, f))

let res3 = eval2_multi (App (if_,  t))


