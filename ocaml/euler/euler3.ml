(*The type of streams*)
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

(*Compute the head of a stream*)
let hd : 'a stream -> 'a = 
  function | Nil -> failwith "hd" | Cons (h, _) -> h

(*Compute the tail of a stream*)
let tl : 'a stream -> 'a stream = 
  function | Nil -> failwith "tl" | Cons (_, t) -> t ()

(*Produce a stream that is a filter of another*)
let rec filter (f : 'a -> bool) : 'a stream -> 'a stream = function
  | Nil -> Nil
  | Cons (x, g) -> 
    if f x then Cons (x, fun () -> filter f (g ()))
    else filter f (g ())
      
(*Take from a stream until a predicate is no longer satisfied*)
let rec take_while (f : 'a -> bool) : 'a stream -> 'a stream = function
  | Nil -> Nil
  | Cons (x, g) ->
    if f x then Cons (x, fun () -> take_while f (g ()))
    else Nil

(*Sieve of Eratosthenes (of Cyrene)*)
let sift (p : int) : int stream -> int stream =
  filter (fun n -> n mod p <> 0)
let rec sieve (s : int stream) : int stream =
  match s with
  | Nil -> Nil
  | Cons (p, g) -> 
    (* print_int p; print_newline (); *)
    Cons (p, fun () -> sieve (sift p (g ())))
(*Natural numbers from starting from [n]*)
let rec from (n : int) : int stream = Cons (n, fun () -> from (n + 1))

(*Find the largest prime factor of [n] using direct search
  factorization*)
let n : int = 600851475143
let primes : int stream = sieve (from 2)
let prime_factors : int stream = 
  filter (fun x -> n mod x = 0) 
  (take_while (fun x -> x <= int_of_float (floor (sqrt (float_of_int n)))) primes)
let largest (l : int stream) : int =
  let rec loop curr = function
    | Nil -> curr
    | Cons (h, g) -> loop h (g ()) in
  let res = loop 0 l in if res <> 0 then res else n
let () = Printf.printf "%d\n" (largest prime_factors)
