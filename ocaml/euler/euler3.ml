(*The type of streams*)
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

(*Compute the head of a stream*)
let hd : 'a stream -> 'a = 
  function | Nil -> failwith "hd" | Cons (h, _) -> h

(*Compute the tail of a stream*)
let tl : 'a stream -> 'a stream = 
  function | Nil -> failwith "tl" | Cons (_, t) -> t ()

(*Get the first [n] elements from a stream *)
let take (n : int) (l : 'a stream) : ' a list =
  let rec loop acc i s =
    match (i, s) with
    | (n, _) when n < 0 -> invalid_arg "negative index in take"
    | (n, _) when n = 0 -> acc
    | (_, Nil) -> acc
    | (n, Cons (h, t)) -> loop (h :: acc) (n - 1) (t ()) in
  List.rev (loop [] n l)

(*Produce a stream that is a filter of another*)
let rec filter (f : 'a -> bool) : 'a stream -> 'a stream = function
  | Nil -> Nil
  | Cons (x, g) -> 
    if f x then Cons (x, fun () -> filter f (g ()))
    else filter f (g ())
      
(*Take from a stream until a predicate is no longer satisfied*)
let rec take_while f = function
  | Nil -> Nil
  | Cons (x, g) ->
    if f x then Cons (x, fun () -> take_while f (g ()))
    else Nil

(*Sieve of Eratosthense*)
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

(*Primes*)
let primes : int stream = sieve (from 2)

(*Find the largest prime factor of [n] using direct search
  factorization*)
let n : int = 600851475143
let prime_factors : int stream = 
  filter (fun x -> n mod x = 0) 
  (take_while (fun x -> x <= int_of_float (floor (sqrt (float_of_int n)))) primes)

let largest (l : int stream) : int =
  let rec loop curr = function
    | Nil -> curr
    | Cons (h, g) -> loop h (g ()) in
  let res = loop 0 l in if res <> 0 then res else n

let () = Printf.printf "%d\n" (largest prime_factors)
