type 'a tree = E | N of 'a tree * 'a * 'a tree

type 'a path =
  | Root
  | Left of 'a * 'a tree * 'a path
  | Right of 'a tree * 'a * 'a path

type 'a cursor = 'a tree * 'a path

exception Top
exception Bottom

let make_tree : 'a tree * 'a * 'a tree -> 'a tree = 
  fun (l, v, r) -> N (l, v, r)

let move_left : 'a cursor -> 'a cursor = fun (tree, path) ->
  match tree with
  | E -> raise Bottom
  | N (l, v, r) -> (l, Left (v, r, path))

let move_right : 'a cursor -> 'a cursor = fun (tree, path) ->
  match tree with
  | E -> raise Bottom
  | N (l, v, r) -> (r, Right (l, v, path))

let move_up : 'a cursor -> 'a cursor = fun (tree, path) ->
  match path with
  | Root -> raise Top
  | Left (v, r, tail) -> (make_tree (tree, v, r), tail)
  | Right (l, v, tail) -> (make_tree (l, v, tree), tail)

let of_tree : 'a tree -> 'a cursor = function
  | E -> failwith "of_tree"
  | N (l, v, r) as n -> (n, Root)

let rec to_tree : 'a cursor -> 'a tree = function
  | (t, Root) -> t
  | (l, Left (v, r, path)) ->
    to_tree (make_tree (l, v, r), path)
  | (r, Right (l, v, path)) ->
    to_tree (make_tree (l, v, r), path)

let replace : 'a cursor -> 'a tree -> 'a cursor =
  fun c t -> 
    match c with
    | _, Root -> (t, Root)
    | (_, Left (v, r, path)) -> (t, Left (v, r, path))
    | (_, Right (l, v, path)) ->  (t, Right (l, v, path))

let delete : 'a cursor -> 'a cursor =  fun c -> replace c E

let most_left (t : 'a tree) : 'a cursor = 
  let rec left : 'a cursor -> 'a cursor = function
    | E, p -> failwith "left"
    | (N (E, _, _), p) as c -> c
    | N (l, x, r), p -> left (l, Left (x, r, p))in
  left (t, Root)

let most_right (t : 'a tree) : 'a cursor = 
  let rec right : 'a cursor -> 'a cursor = function
    | E, p -> failwith "right"
    | N (_, _, E), p as c -> c
    | N (l, x, r), p -> right (r, Right (l, x, p)) in
  right (t, Root)

let first_leaf (t : 'a tree) : 'a cursor =
  let rec down : 'a cursor -> 'a cursor = function
    | E, p -> failwith "first_leaf"
    | (N (E, _, E), p) as c -> c
    | N (E, x, r), p -> down (r, Right (E, x, p))
    | N (l, x, r), p -> down (l, Left (x, r, p))
  in down (of_tree t)

let next_leaf ((t, p) : 'a cursor) : 'a cursor =
  let rec down (p : 'a path) : 'a tree -> 'a cursor = function
    | E -> raise Bottom
    | N (E, _, E) as leaf -> (leaf, p)
    | N (E, x, r) -> down (Right (E, x, p)) r
    | N (l, x, r) -> down (Left (x, r, p)) l
  in
  let rec up (t : 'a tree) : 'a path -> 'a cursor = function
    | Root -> raise Top
    | Left (x, E, p) -> up (make_tree (t, x, E)) p
    | Left (x, r, p) -> down (Right (t, x, p)) r
    | Right (l, x, p) -> up (make_tree (l, x, t)) p
  in
  up t p
    
let last_leaf (t : 'a tree) : 'a cursor =
  let rec down : 'a cursor -> 'a cursor = function
    | E, p -> failwith "last_leaf"
    | (N (E, _, E), p) as c -> c
    | N (l, x, E), p -> down (l, Left (x, E, p))
    | N (l, x, r), p -> down (r, Right (l, x, p))
  in down (of_tree t)

let prev_leaf ((t, p) : 'a cursor) : 'a cursor =
  let rec down (p : 'a path) : 'a tree -> 'a cursor = function
    | E -> raise Bottom
    | N (E, _, E) as leaf -> (leaf, p)
    | N (l, x, E) -> down (Left (x, E, p)) l
    | N (l, x, r) -> down (Right (l, x, p)) r
  in
  let rec up (t : 'a tree) : 'a path -> 'a cursor = function
    | Root -> raise Top
    | Right (E, x, p) -> up (make_tree (E, x, t)) p
    | Right (l, x, p) -> down (Left (x, t, p)) l
    | Left (x, r, p) -> up (make_tree (t, x, r)) p
  in 
  up t p

let collect_leaves (t : 'a tree) : 'a list =
  let rec aux acc ((N(_, x, _), p) as c) =
    let leaves = x :: acc in   
    try
      aux leaves (next_leaf c)
    with _ -> leaves in
  List.rev@@ aux [] (first_leaf t)
  
(*
       1
     2   3
   4   5
*)
let t =  make_tree (make_tree (
  make_tree (E, 4, E), 2, make_tree (E, 5, E)), 1, make_tree (E, 3, E))
(*
       1
     2   3
   4
*)
let t = make_tree (make_tree (
  make_tree (E, 4, E), 2, E), 1, make_tree (E, 3, E))
(*
       1
         2
           3
*)
let t = make_tree (E, 1, make_tree (E, 2, make_tree (E, 3, E)))
(*
       1
     2
   3
*)
let t = make_tree(make_tree (make_tree (E, 3, E), 2, E), 1, E)

