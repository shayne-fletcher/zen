type 'a impl = 
['a Var.impl | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a]

type t = 'a impl as 'a

let mk_num (i : int) : [> `Num of 'a] = `Num i
let mk_add ((u : 'a), (v : 'b)) : [> `Add of 'a * 'b] = `Add (u, v)
let mk_mult ((u : 'a), (v : 'b)) : [> `Mult of 'a * 'b] = `Mult (u, v)

let string_of_impl (string_of_rec : 'a -> string) : 'a impl -> string = 
  function
  | #Var.t as v ->
    Var.string_of_impl string_of_rec v
  | `Num i -> 
    string_of_int i
  | `Add (u, v) -> 
    "(" ^ (string_of_rec u) ^ " + " ^ (string_of_rec v) ^ ")"
  | `Mult (u, v) -> 
    "(" ^ (string_of_rec u) ^ " * " ^ (string_of_rec v) ^ ")"

let rec string_of_t : t -> string = fun v -> string_of_impl string_of_t v

module Detail = struct 
  let map_arith 
      (f : (
        [> `Var of string | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a]
          as 'a) -> 'a)  : 'a impl -> 'a = function
      | #Var.t as v -> v
      | `Num _ as n -> n
      | `Add (e1, e2) -> `Add (f e1, f e2)
      | `Mult (e1, e2) -> `Mult (f e1, f e2)
end

let eval_impl eval_rec env (e : 'a impl) : 'a =
  match Detail.map_arith (eval_rec env) e with
  | #Var.t as v -> Var.eval_impl env v
  | `Add (`Num m, `Num n) -> `Num (m + n)
  | `Mult (`Num m, `Num n) -> `Num (m * n)
  | e -> e

let rec eval (env : (string * t) list) : 'a -> 'a  =
  eval_impl eval env
