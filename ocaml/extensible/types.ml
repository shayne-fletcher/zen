module Var = struct
  type var = [`Var of string]
  type t = var

  let mk_var : string -> [> `Var of string] = fun s -> `Var s

  (*Evaluation of a variable means looking in an environment for a
    binding and leaving it "as is" if there isn't one*)
  let eval_var 
      (env : (string * ([> `Var of string ] as 'a)) list ) 
      (`Var s as v : var) : 'a =
    try
      List.assoc s env
    with
    | Not_found -> v

  let string_of_var : t -> string = function
    | `Var s -> s
end

module Lambda = struct
  type 'a lambda = [Var.var | `Abs of string * 'a | `App of 'a * 'a]
  type t = 'a lambda as 'a

  let mk_abs : string * 'b -> [> `Abs of string * 'b] = fun (s, t) -> `Abs (s, t) 
  let mk_app : 'a * 'b -> [> `App of 'a * 'b] = fun (u, v) -> `App (u, v)

  let gen_sym =
    let n = ref 0 in
    fun () -> incr n; "_" ^ string_of_int !n

  (*Evaluation of lambda expressions*)
  let eval_lambda eval_rec env : 'a lambda -> 'a = function
    | #Var.t as v ->
      Var.eval_var env v
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
  let rec eval1 (env : (string * t) list) : t -> t = 
    eval_lambda eval1 env

  module Detail = struct
    let rec strip (bs : string list) (t : t) : string list * t =
      match t with 
      | `Abs (b, t) -> strip (b :: bs) t
      | _ as u -> (List.rev bs, u)

  end (*module Detail*)

  let rec string_of_lambda : t -> string = function
    | #Var.t as v -> 
      Var.string_of_var v
    | `App (u, v) -> 
      "("^(string_of_lambda u) ^ ") (" ^ (string_of_lambda v)^")"
    | `Abs _ as t -> 
      match (Detail.strip [] t) with
      | (b :: bs, u)  ->
        let binder = 
          "\\" ^ b ^ (List.fold_right  (fun z b -> " " ^ z ^ b) bs ". ") in
        binder^(string_of_lambda u)
      | ([], _) -> assert false

end
