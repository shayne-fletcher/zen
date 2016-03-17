type 'a impl = [`Var of string]
type t = 'a impl as 'a

let mk_var : string -> [> `Var of string] = fun s -> `Var s

let string_of_impl (_ : 'a -> string) : 'a impl -> string = function 
  | `Var s -> s

let rec string_of_t : t -> string = fun v -> string_of_impl string_of_t v

let eval_impl
    (env : (string * ([> `Var of string ] as 'a)) list) 
    (`Var s as v : 'a impl) : 'a =
  try
    List.assoc s env
  with
  | Not_found -> v
