module Var = struct
  type 'a impl = [`Var of string]
  type t = 'a impl as 'a

  let mk_var : string -> [> `Var of string] = fun s -> `Var s

  let string_of_impl (_ : 'a -> string) : 'a impl -> string = function
    | `Var s -> s

  let eval_var 
      (env : (string * ([> `Var of string ] as 'a)) list ) 
      (`Var s as v : 'a impl) : 'a =
    try
      List.assoc s env
    with
    | Not_found -> v

end

module Lambda = struct
  type 'a impl = ['a Var.impl | `Abs of string * 'a | `App of 'a * 'a]
  type t = 'a impl as 'a

  let mk_app : 'a * 'b -> [> `App of 'a * 'b] = fun (u, v) -> `App (u, v)
  let mk_abs : string * 'b -> [> `Abs of string * 'b] = fun (s, t) -> `Abs (s, t) 

  module Detail = struct
    let gen_sym =
      let n = ref 0 in
      fun () -> incr n; "_" ^ string_of_int !n

    let rec strip (bs : string list) t =
      match t with 
      | `Abs (b, t) -> strip (b :: bs) t
      | _ as u -> (List.rev bs, u)

  end (*module Detail*)

  let string_of_impl (string_of_rec : 'a -> string) : 'a impl -> string = 
    function
    | #Var.t as v -> Var.string_of_impl string_of_rec v
    | `App (u, v) -> 
      "("^(string_of_rec u) ^ ") (" ^ (string_of_rec v)^")"
    | `Abs _ as t -> 
      match (Detail.strip [] t) with
      | (b :: bs, u)  ->
        let binder = 
          "\\" ^ b ^ (List.fold_right  (fun z b -> " " ^ z ^ b) bs ". ") in
        binder ^ (string_of_rec u)
      | ([], _) -> assert false

  let eval_lambda eval_rec env : 'a impl -> 'a = function
    | #Var.t as v ->
      Var.eval_var env v
    | `App (u, v) ->
      let v' = eval_rec env v in
      begin match eval_rec env u with
      | `Abs (s, body) -> 
        eval_rec [s, v'] body
      | u' -> `App (u', v')
      end
    | `Abs (s, u) ->
      let s' = Detail.gen_sym () in
      `Abs (s', eval_rec ((s, `Var s') :: env) u)

  let rec eval (env : (string * t) list) : t -> t = 
    eval_lambda eval env

end

module Expr = struct

  type 'a impl = ['a Var.impl | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a]
  type t = 'a impl as 'a

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

  let mk_num (i : int) : [> `Num of 'a] = `Num i
  let mk_add ((u : 'a), (v : 'b)) : [> `Add of 'a * 'b] = `Add (u, v)
  let mk_mult ((u : 'a), (v : 'b)) : [> `Mult of 'a * 'b] = `Mult (u, v)

  let map_expr 
      (f : (
        [> `Var of string | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a]
          as 'a) -> 'a)  : 'a impl -> 'a = function
    | #Var.t as v -> v
    | `Num _ as n -> n
    | `Add (e1, e2) -> `Add (f e1, f e2)
    | `Mult (e1, e2) -> `Mult (f e1, f e2)

  let eval_expr eval_rec env (e : 'a impl) : 'a =
    match map_expr (eval_rec env) e with
    | #Var.t as v -> Var.eval_var env v
    | `Add (`Num m, `Num n) -> `Num (m + n)
    | `Mult (`Num m, `Num n) -> `Num (m * n)
    | e -> e

  let rec eval (env : (string * t) list) : 'a -> 'a  =
    eval_expr eval env

end

module Lambda_with_arithmetic = struct
  type 'a impl = ['a Var.impl | 'a Lambda.impl | 'a Expr.impl]
  type t = 'a impl as 'a

  let eval_lexpr eval_rec 
      (env : (string *
                ([> `Abs of string * 'a
                 | `Add of 'a * 'a
                 | `App of 'a * 'a
                 | `Mult of 'a * 'a
                 | `Num of int
                 | `Var of string ]
                    as 'a)) list) : 'a impl -> 'a = function
      | #Lambda.impl as x -> Lambda.eval_lambda eval_rec env x
      | #Expr.impl as x -> Expr.eval_expr eval_rec env x

  let rec string_of_t : t -> string = 
    function
    | #Expr.impl as e -> Expr.string_of_impl string_of_t e
    | #Lambda.impl as l -> Lambda.string_of_impl string_of_t l

  let rec eval (env : (string * t) list) : t -> t =
    eval_lexpr eval env
    
end
