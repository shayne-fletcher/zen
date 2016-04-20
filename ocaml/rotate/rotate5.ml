type 'a stream =  Nil | Cons of 'a * (unit -> 'a stream)

let hd : 'a stream -> 'a = 
  function | Nil -> failwith "hd" | Cons (h, _) -> h

let tl : 'a stream -> 'a stream = 
  function | Nil -> failwith "tl" | Cons (_, t) -> t ()

let rec nth (s : 'a stream) (n : int) : 'a =
  if n = 0 then hd s else nth (tl s) (n - 1)

let rec take (lst : 'a stream) (n : int) : 'a list = 
  match (n, lst) with
  | (n, _) when n < 0 -> invalid_arg "negative index in take"
  | (n, _) when n = 0 -> []
  | (_, Nil) -> []
  | (n, Cons (h, t)) -> h :: (take (t ()) (n - 1))

(*
  let rec from n -> n :: from (n + 1)
*)
let rec from (n : int)  : int stream =
  Cons (n, fun () -> from (n + 1))

let natural_numbers : int stream = from 0

(*
  let rec filter p = function
  | [] -> []
  | (h :: tl) ->
      if p h then h :: filter p tl
      else  filter p tl
*)
let rec filter (p : 'a -> bool) : 'a stream -> 'a stream = function
  | Nil -> Nil
  | Cons (h, t) ->
    if p h then Cons (h, fun () -> filter p (t ()))
    else  filter p (t ())

let sift (p : int) (s : int stream) : int stream = 
  filter (fun n -> n mod p <> 0) s

(*
  let rec sieve = function
    | [] -> []
    | (h :: tl) -> h :: sieve (sift h tl)
*)
let rec sieve = function
  | Nil -> Nil
  | Cons (h, t) -> Cons (h, fun () -> sieve (sift h (t ())))

let primes : int stream = sieve (from 2)

let p = nth primes 1000

(*Compute the 999th prime number
  nth primes 1000
*)

