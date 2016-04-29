(*Ruka Morgan*)

let rec cursor_most_left : 'a cursor -> 'a cursor = fun c ->
  try 
     cursor_most_left (move_left c) 
  with 
    | Bottom -> move_up c

let rec cursor_most_right : 'a cursor -> 'a cursor = fun c ->
  try 
    cursor_most_right (move_right c)
  with 
    | Bottom -> move_up c 

let rec cursor_first_leaf : 'a cursor -> 'a cursor = fun c ->
  match c with 
  | N(E,_,E),_ -> c
  | N(E,_,_),_  -> cursor_first_leaf (move_right c)
  | N(_,_,_),_ -> cursor_first_leaf (cursor_most_left c)
  | _ -> failwith "cursor_first_leaf" 

let rec cursor_last_leaf : 'a cursor -> 'a cursor = fun c ->
  match c with 
  | N(E,_,E),_ -> c 
  | N(_,_,E),_ -> cursor_last_leaf (move_left c)
  | N(_,_,_),_ -> cursor_last_leaf (cursor_most_right c) 
  | _ -> failwith "cursor_last_leaf"

let value_of_cursor : 'a cursor -> 'a = fun c ->
  match c with 
  | N(_,v,_),_ -> v 
  | _ -> failwith "value_of_cursor"

let most_left : 'a tree -> 'a cursor =  fun(t) ->
  cursor_most_left (of_tree t) 

let most_right : 'a tree -> 'a cursor = fun(t) -> 
  cursor_most_right (of_tree t)

let first_leaf : 'a tree -> 'a cursor = fun(t) ->
  cursor_first_leaf(of_tree t)

let rec next_leaf : 'a cursor -> 'a cursor = fun(c) -> 
  try
    match c with 
    | _,Right(_) -> next_leaf (move_up c)
    | _,Left (_) -> 
      c 
      |> move_up
      |> move_right
      |> cursor_first_leaf 
    | _, Root -> raise Top
  with 
    | _ -> failwith "next_leaf"

let last_leaf : 'a tree -> 'a cursor = fun(t) ->
  cursor_last_leaf (of_tree t) 

let rec prev_leaf : 'a cursor -> 'a cursor = fun c -> 
  try 
    match c with 
    | _, Left (_) -> prev_leaf (move_up c)
    | _, Right (_) -> 
      c
      |> move_up
      |> move_left 
      |> cursor_last_leaf 
    | _, Root -> raise Top
  with 
    | _ -> failwith "prev_leaf"

let collect_leaves : 'a tree -> 'a list = fun(t) ->
  let rec try_add_prev : 'a cursor list -> 'a cursor -> 'a cursor list = fun  acc current ->
    try
      let p = prev_leaf current in 
      try_add_prev (p::acc) p 
    with 
      | _ -> acc 
  in 
  let l = last_leaf t in
  let clist = try_add_prev [l] l in 
  List.map value_of_cursor clist


(*Thomas Foster*)

(* revised submission (altered definition of `apply_until_raise` to be tail-recursive) -- thanks Shayne! *)

let forbid_empty_selection = fun op c ->
    match (op c) with
    | (E, _) -> raise Bottom
    | _ as x -> x

let rec apply_until_raise f e a =
    let maybe_next = try Some (f a) with e -> None in
    match maybe_next with
    | None    -> a
    | Some fa -> apply_until_raise f e fa

(* exported *)
let most_left t = apply_until_raise (forbid_empty_selection move_left) Bottom (of_tree t)

(* exported *)
let most_right t = apply_until_raise (forbid_empty_selection move_right) Bottom (of_tree t)

let left_if_possible_otherwise_right c =
    try (forbid_empty_selection move_left) c
    with Bottom -> (forbid_empty_selection move_right) c

let right_if_possible_otherwise_left c =
    try (forbid_empty_selection move_right) c
    with Bottom -> (forbid_empty_selection move_left) c

let first_leaf_cursor c = apply_until_raise left_if_possible_otherwise_right Bottom c

let last_leaf_cursor c = apply_until_raise right_if_possible_otherwise_left Bottom c

(* exported *)
let first_leaf t = first_leaf_cursor (of_tree t)

(* exported *)
let last_leaf t = last_leaf_cursor (of_tree t)

(* exported *)
let rec next_leaf : 'a cursor -> 'a cursor = function
    | (_, Left (_, N (_, _, _), _)) as c -> first_leaf_cursor (move_right (move_up c))
    | _                             as c -> next_leaf (move_up c)

(* exported *)
let rec prev_leaf : 'a cursor -> 'a cursor = function
    | (_, Right (N (_, _, _), _, _)) as c -> last_leaf_cursor (move_left (move_up c))
    | _                              as c -> prev_leaf (move_up c)

let get_leaf_value ((N (E, v, E)), _) = v

let collect_leaves_helper (l, c) = let prev = prev_leaf c in (get_leaf_value prev :: l, prev)

(* exported *)
let collect_leaves = function | E -> [] | _ as t ->
    let last = last_leaf t in
    let rbegin = ([get_leaf_value last], last) in
    let (l, c) = apply_until_raise collect_leaves_helper Top rbegin in l
