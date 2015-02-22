type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

let rec (filter : ('a -> bool) -> 'a stream -> 'a stream) = 
  fun p -> function
  | Nil -> Nil
  | Cons (x, g) ->
    if p x then Cons (x, fun () -> filter p (g ()))
    else filter p (g())

let rec take (s : 'a stream) (n : int) : 'a list = 
  match (n, s) with
  | (n, _) when n < 0 -> invalid_arg "negative index in take"
  | (n, _) when n = 0 -> []
  | (_, Nil) -> []
  | (n, Cons (h, g)) -> h :: (take (g ()) (n - 1))

let sift (p : int) : int stream -> int stream =  filter (fun n -> n mod p <> 0)

let rec sieve : int stream -> int stream = function
  | Nil -> Nil
  | Cons (p, g) -> Cons (p, fun () -> sieve (sift p (g ())))

let rec from (n : int) : int stream = Cons (n, fun () -> from (n + 1))
  
let primes = sieve (from 2)
