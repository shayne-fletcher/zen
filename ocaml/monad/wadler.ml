(*List monad*)

let map : ('a -> 'b) -> ('a list -> 'b list) = fun f -> fun l -> List.map f l
let unit : ('a -> 'a list) = fun x -> x :: []
let join : ('a list list) -> 'a list = fun ls -> List.concat ls

(*Theorems for free*)

(*map f o unit = unit o f*)
(*i*)
let (g : ('a -> 'b) -> 'a -> 'b list) = fun f -> fun x -> (map f) (unit x)
(*ii*)
let (h : ('a -> 'b) -> 'a -> 'b list) = fun f -> fun x -> unit (f x)

(*map f o join = join o map (map f)*)
(*iii*)
let (i : ('a -> 'b) -> 'a list list -> 'b list) = fun f -> fun ls -> (map f) (join ls)
(*iv*)
let (j :  ('a -> 'b) -> 'a list list -> 'b list) = fun f -> fun ls -> join (map (map f) ls)

(*List comprehensions*)

(*
  Comprehensions are defined by the following rules:

  (1) [t | \Gamma] = unit t,
  (2) [t | x <- u] = map (fun x -> t) u,
  (3) [t | (p, q)] = join [[t | q] | p]
*)

(*e.g. [(x, y) | x <- [1, 2], y <- [3, 4]] 
   = {(3)}
     join [[t | y <- [3; 4]] | x <- [1; 2]]
   = {(2)}
    join [map (fun y -> (x, y)) [3; 4] | x <- [1; 2]]
   = {(2)}
    join [map (fun x -> (fun y -> (x, y)) [3; 4]) [1; 2]]
*)
let l = join (map (fun x -> map (fun y -> (x, y))[3; 4]) [1; 2])
(*That is, 'val l : (int * int) list = [(1, 3); (1, 4); (2, 3); (2, 4)]'*)

(*From (i).. (iv) and (1).. (3) further laws can be derived*)
(*
  (4) [f t | q] = map f [t | q]
  (5) [x | x <- u] = u
  (6) [t | p, x <- [u | q], r] = [t_{x}^{u} | p, q, r_{x}^{u}]

  In (4), 'f' may not contain free occurences of variables bound by
  'q' and in (6), 't_{x}^{u}' stands for term 't' with term 'u'
  substituted for each free occurence of variable 'x' and similarly
  for the qualifier 'r_{x}^{u}'.
*)

(*The identity monad*)

type 'a id = 'a
let (map_id : ('a -> 'b) -> ('a id -> 'b id)) = fun f -> fun x -> f x
let (unit_id : 'a -> 'a id) = fun x -> x
let (join : ('a id) id -> 'a id) = fun x -> x

(*
   It seems:

    - join xs = xs * id
    - a * k = join ((map k) a)

   Confirm!

*)
