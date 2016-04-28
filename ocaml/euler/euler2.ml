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
      
(*Take elements from a stream while they satisfy a predicate*)
let take_while (p : 'a -> bool) (l : 'a stream) : 'a list =
  let rec loop acc s =
    match s with
    | Nil -> acc
    | Cons (h, t) -> 
      if p h then loop (h :: acc) (t ()) else acc
  in
  List.rev (loop [] l)

(*The Fibonacci sequence*)
let fibonacci_numbers : int stream =
  let rec fib a b = Cons (a, fun () -> fib b (a + b)) in
fib 0 1

(*The even Fibonacci numbers less than 4,000,000*)
let l : int list = 
  take_while 
    (fun x -> x < 4000000) 
    (filter (fun x -> x mod 2 = 0) fibonacci_numbers)

(*The sum of the even Fibonacci numbers less than 4,000,000*)
let sum = List.fold_left (fun x y -> x + y) 0 l
