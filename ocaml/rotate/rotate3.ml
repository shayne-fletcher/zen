type 'a stream =  Nil | Cons of 'a * (unit -> 'a stream)

let hd = function | Nil -> failwith "hd" | Cons (h, _) -> h
let tl = function | Nil -> failwith "tl" | Cons (_, t) -> t ()

let rec repeat (x : 'a) : 'a stream = Cons (x, fun () -> repeat x)

(*Factory function to compute a stream from a list*)
let from_list (l : 'a list) : 'a stream =
  List.fold_right (fun x s -> Cons (x, (fun () -> s))) l Nil

(* For eager lists, we can write a function that appends two lists
   'join' like this.

   ```
     let rec join l m =
       match l with
       | [] -> m
       | h :: t -> h :: (join t m)

   ```

   This generalizes naturally to streams.
*)
let rec join (l : 'a stream) (m : 'a stream) =
  match l with
  | Nil -> m
  | Cons (h, t) -> Cons (h, (fun () -> join (t ()) m))

(* For eager lists, we can write 'flatten' in terms of 'join'

   ```
     let rec flatten : 'a list list -> 'a list = function
     | [] -> []
     | (h :: tl) -> join h (flatten tl)

   ```

   Emboldened by our earlier success we might try to generalize it to
   streams like this.

   ```
     let rec flatten (l : 'a stream stream) : 'a stream =
     match l with
     | Nil -> lazy Nil
     | Cons (l, r) ->  join l (flatten (r ()))

   ```

   Sadly, no. This definition results in stack overflow.

   There is an alternative eager phrasing of 'flatten' we might try.
   ```
     let rec flatten = function
       | [] -> []
       | [] :: t -> flatten t
       | (x :: xs) :: t -> x :: (flatten (xs :: t))

   ```

   Happy to say, this one generalizes and gets around the eager
   evaluation problem that causes the unbounded recursion.
*)

let rec flatten : 'a stream stream -> 'a stream = function
  | Nil -> Nil
  | Cons (Nil, t) -> flatten (t ())
  | Cons (Cons (x, xs), t) -> 
    Cons (x, fun () -> flatten (Cons (xs (), t)))

(*'take' and 'drop' are straight forward generalizations of their
  eager counterparts*)

let rec drop (n : int) (lst : 'a stream ) : 'a stream = 
  match (n, lst) with
  | (n, _) when n < 0 -> invalid_arg "negative index in drop"
  | (n, xs) when n = 0 -> xs
  | (_, Nil) -> Nil
  | (n, Cons (_, t)) -> drop (n - 1) (t ())

let rec take (n : int) (lst : 'a stream) : 'a list = 
  match (n, lst) with
  | (n, _) when n < 0 -> invalid_arg "negative index in take"
  | (n, _) when n = 0 -> []
  | (_, Nil) -> []
  | (n, Cons (h, t)) -> h :: (take (n - 1) (t ()))

(*Which brings us to the 'rotate' functions given to us by Joel in
  Haskell

  ```
    rotateLeft n xs 
    | n >= 0     = take (length xs) $ drop n $ concat $ repeat xs
    | otherwise  = rotateLeft (length xs + n) xs

    rotateRight n = rotateLeft (-n)

  ```

  They render as follows in about the same number of lines.
*)

let rec rotate_left (k : int) (l : 'a list) : 'a list =
  let n = List.length l in
  if k >= 0 then
    l |> from_list |> repeat |> flatten |> drop n |> take n
  else rotate_left (n + k) l

let rotate_right (n : int) : 'a list -> 'a list = rotate_left (-n)

(*By the way, here's a little 'hack' for writing infinite lists

  - To start, forget about streams
  - Write your list using regular lists
  - Ignore the fact that it won't terminate
  - Rewrite in terms of Cons and Nil and convert the tail to a thunk

  e.g. 

  This pseudo-code
  ```
    let fibonacci_numbers = 
      let rec fib a b = a :: fib b (a + b) in
      fib 0 1

  ```
  leads to

  ```
    let fibonnaci_sequence = 
      let rec fib a b = Cons (a, lazy (fib b (a + b))) in
    fib 0 1

  ```

  This pseudo-code
  ```
    let naturals = 
      let rec loop x = x :: loop (x + 1)in
    next 0

  ```

  leads to 

  ```
    let natural_numbers =
      let rec loop x = Cons (x, lazy (loop (x + 1))) in
      loop 0

  Neato.
*)

let fibonacci_sequence = 
    let rec fib a b = Cons (a, fun () -> fib b (a + b)) in
fib 0 1

