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
let rec from (n : int) : int stream =  Cons (n, fun () -> from (n + 1))
(*The (infinite) stream of prime numbers*)
let primes : int stream = sieve (from 2)

(*Find the largest prime factor of [n] using direct search
  factorization*)
let trial (n : int) : int list =
  let candidates : int stream = 
    take_while 
      (fun x -> 
       x <=  int_of_float (floor (sqrt (float_of_int n)))) 
      primes in
  let rec loop (k, acc) = function
    | Cons (p, g) -> 
       if p * p > k then 
         (k , acc)
       else
         let rec aux (k, acc) =
           if k mod p = 0 then
             aux ((k / p), (p :: acc))
           else (k, acc) in
         loop (aux (k, acc)) (g ()) in
  let i, l = loop (n, []) candidates in
  if i > 1 then i :: l else l
(*
  In the top level:
    #load "unix.cma" ;;

  To compile:
    $ ocamlopt.opt -o euler3.exe unix.cmxa euler3.ml
*)
let time (f : unit -> 'a ) : 'a = 
  let t = Unix.gettimeofday () in
  let res = f () in
  let () = Printf.printf "Time : %f (s)\n" (Unix.gettimeofday () -. t) in
  res
let factors : int list =
  time (fun () -> trial 600851475143)
