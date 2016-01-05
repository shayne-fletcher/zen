(*List monad*)

let map : ('a -> 'b) -> ('a list -> 'b list) = fun f -> fun l -> List.map f l
let unit : ('a -> 'a list) = fun x -> x :: []
let join : ('a list list) -> 'a list = fun ls -> List.concat ls
let ( * ) : 'a list -> ('a -> 'b list) -> 'b list = fun a -> fun k -> join ((map k) a)

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

(*e.g. [(x, y) | x <- [1; 2], y <- [3; 4]] 
   = {(3)}
     join [[t | y <- [3; 4]] | x <- [1; 2]]
   = {(2)}
    join [map (fun y -> (x, y)) [3; 4] | x <- [1; 2]]
   = {(2)}
    join [map (fun x -> (fun y -> (x, y)) [3; 4]) [1; 2]]
*)
let l = join (map (fun x -> map (fun y -> (x, y))[3; 4]) [1; 2])
(*That is, 'val l : (int * int) list = [(1, 3); (1, 4); (2, 3); (2, 4)]'*)

(*Using join z = z * id
*)
let l' = (map (fun x -> map (fun y -> (x, y))[3; 4]) [1; 2]) * (fun x -> x)
(*
  Using map f m = m * \a.unit (f a) we have

  map (fun y -> (x, y))[3; 4] = [3; 4] * (fun y -> unit (x, y))
*)
let l'' = map (fun x -> [3; 4] * (fun y -> unit (x, y))) [1; 2] * (fun x -> x)
(* and applying the same rule to the outer map

  map (fun x -> [3; 4] * (fun y -> unit (x, y))) [1; 2] =
  [1; 2] * (fun a -> unit ((fun x -> [3; 4] * (fun y -> unit (x, y))) a)
*)
let l''' = [1; 2] * (fun a -> unit ((fun x -> [3; 4] * (fun y -> unit (x, y))) a)) * (fun x -> x)

(*
   Rephrase the comprehension rules in terms of bind:

  (1') [t | x <- u] = u * (fun x -> unit t)

    Proof:

    [t | x <- u] 
      = {2}
        map (fun x -> t) u
      =  {defn. : map f m = m * fun a -> unit (f a)} 
        u * (fun x -> unit ((fun x -> t) x))
      = {simplifying}
        u * fun x -> unit t

  (2') [t | (p, q)] = p * fun x -> q * fun y -> unit t
    
   Proof:

     [t | (p, q)]
     = {3}
       join [[t | q] | p]
     = {(1')}
       join [(q * (fun y -> unit t)) | p]
     = {2}
       join (map (fun x -> (q * (fun y -> unit t))) p)
     = {defn. : a * k = join ((map k) a)}
       p * fun x -> q * (fun y -> unit t)

*)


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

    - a * k = join ((map k) a)
    - join z = z * \m.m (i.e. join z = z * id)

   Confirm!

*)
