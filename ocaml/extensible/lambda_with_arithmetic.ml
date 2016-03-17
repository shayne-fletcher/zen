type 'a impl = ['a Var.impl | 'a Lambda.impl | 'a Arith.impl]
type t = 'a impl as 'a

let eval_larith eval_rec 
    (env : (string *
              ([> `Abs of string * 'a
               | `Add of 'a * 'a
               | `App of 'a * 'a
               | `Mult of 'a * 'a
               | `Num of int
               | `Var of string ]
                  as 'a)) list) : 'a impl -> 'a = function
    | #Lambda.impl as x -> Lambda.eval_lambda eval_rec env x
    | #Arith.impl as x -> Arith.eval_arith eval_rec env x

let string_of_impl (string_of_rec : 'a -> string) : 'a impl -> string =
  function
  | #Var.impl as v -> Var.string_of_impl string_of_rec v
  | #Arith.impl as e -> Arith.string_of_impl string_of_rec e
  | #Lambda.impl as l -> Lambda.string_of_impl string_of_rec l
  
let rec string_of_t : t -> string = 
  fun t -> string_of_impl string_of_t t

let rec eval (env : (string * t) list) : t -> t =
  eval_larith eval env
