(*Signature of a basic monad*)
module type M_req = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

(*Signature of a monadic '>>=' operator*)
module type Infix = sig
  type 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(*Signature of a monad that supports '>>='*)
module type S = sig
  include Infix
  module Monad_infix : Infix with type 'a t := 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

(*A 'functor' that maps a basic monad to a monad that supports '>>='*)
module Make (M : M_req) : S with type 'a t := 'a M.t = struct

  let bind = M.bind
  let return = M.return

  module Monad_infix = struct
    let ( >>= ) = bind
  end

  include Monad_infix

end

(*Test*)

type 'a success =
  | Success of 'a
  | Failure of string

let string_of_success f s =
  match s with
  | Success a -> Printf.sprintf "Success '%s'" (f a)
  | Failure s -> Printf.sprintf "Failure '%s'" s

let madd x y =
  if x > 0 && y > (max_int - x) then
      Failure "overflow"
  else
    Success (y + x)

let msub x y =
  if x > 0 && y < (min_int + x) then
    Failure "underflow"
  else
    Success (y - x)

let mmul x y =
  if x != 0 &&  y > (max_int / x) then
    Failure "overflow"
  else
    Success (y * x)

let mdiv x y =
  if (x = 0) then
    Failure "attempted division by zero"
    else
    Success (y / x)

(*Here's a basic monad*)
module Basic : M_req with type 'a t = 'a success = struct
  type 'a t = 'a success

  let bind a f =
    match a with
    | Success x -> f x
    | Failure s -> Failure s

  let return x = Success x
end

module M = Make (Basic)

(*'main'*)

let () =
  let open M in
  Printf.printf "2 + 3 = %s\n"(string_of_success string_of_int (return 2 >>= madd 3))

