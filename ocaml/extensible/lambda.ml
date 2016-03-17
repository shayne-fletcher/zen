type 'a impl = ['a Var.impl | `Abs of string * 'a | `App of 'a * 'a]
type t = 'a impl as 'a

let mk_app : 'a * 'b -> [> `App of 'a * 'b] = 
  fun (u, v) -> `App (u, v)
let mk_abs : string * 'b -> [> `Abs of string * 'b] = 
  fun (s, t) -> `Abs (s, t) 

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

let rec string_of_t : t -> string = fun v -> string_of_impl string_of_t v

let eval_impl eval_rec 
    (env : (string *
              ([> 
                 `Abs of string * 'a 
               | `App of 'a * 'a 
               | `Var of string ] as 'a))
       list) : 'a impl -> 'a = function
    | #Var.t as v -> Var.eval_impl env v
    | `App (u, v) ->
      let v' = eval_rec env v in
      begin match eval_rec env u with
      | `Abs (s, body) -> eval_rec [s, v'] body
      | u' -> `App (u', v')
      end
    | `Abs (s, u) ->
      let s' = Detail.gen_sym () in
      `Abs (s', eval_rec ((s, `Var s') :: env) u)

let rec eval (env : (string * t) list) : t -> t = 
  eval_impl eval env
