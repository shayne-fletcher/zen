(*Sen Han*)

let rec most_left_impl ((tr, p) : 'a cursor) : 'a cursor =
  match tr with
  | N(E, v, r) as n ->  (n, p)
  | _ -> (most_left_impl (move_left (tr, p)));;

let most_left (t : 'a tree) : 'a cursor =
  most_left_impl (of_tree t);;

let rec most_right_impl ((tr, p) : 'a cursor) : 'a cursor =
  match tr with
  | N(l, v, E) as n ->  (n, p)
  | _ -> (most_right_impl (move_right (tr, p)))

let most_right (t : 'a tree) : 'a cursor =
  most_right_impl (of_tree t);;

let rec first_leaf_impl ( (tre, p) : 'a cursor) =
  match tre with
  | N(E, v, E) -> (tre, p)
  | N(E, v, r) -> (first_leaf_impl (move_right (tre, p)))
  | _ -> (most_left_impl (tre, p));;

let first_leaf (tr : 'a tree) : 'a cursor =
  first_leaf_impl (of_tree tr);;

let move_up : 'a cursor -> 'a cursor = fun (tree, path) ->
  match path with
  | Root -> raise Top
  | Left(v, t, p) -> (N(tree, v, t),p)
  | Right(t, v, p) -> (N(t, v, tree),p);;

(* let rec next_sub ((t0, p0) : 'a cursor) : 'a cursor = *)
(*   match (t0, p0) with *)
(*   | (N(_, _, E), Root) -> failwith "next_leaf" *)
(*   | (N(_, _, E), _) as c -> (print_string "move_up");(next_sub (move_up c)); *)
(*   | _ as c -> move_right c;; *)

let rec next_sub ((t, p) : 'a cursor) : 'a cursor=
  let has_right_sub_tree (n : 'a tree) : bool =
    match n with
    | N(_, _, E) -> false
    | _  -> true
  in
  match (has_right_sub_tree t) with
  | true -> (move_right (t, p))
  | false -> (next_sub (move_up (t, p)));;

let next_sub_from_left ( c : 'a cursor) : 'a cursor =
  first_leaf_impl (next_sub c);;

let next_sub_from_right ( c0 : 'a cursor) : 'a cursor =
  let rec find_pivot c : 'a cursor =
    match c with
    | (_, Left(v, t, p)) as cur -> cur
    | _ -> (find_pivot (move_up c));
  in
  next_sub_from_left (move_up (find_pivot c0));;

let next_leaf ((tr, path) : 'a cursor) : 'a cursor =
  match path with
  | Left(_) -> (next_sub_from_left (tr, path))
  | Right(_) -> (next_sub_from_right (tr, path))
  | Root -> (tr, path);;

  (*   1 *)
  (*  2 *)
  (* 3 *)
let tree1 =  N(N(N(E, 3, E), 2, E), 1, E);;

(*  1 *)
(*   2 *)
(*    3 *)
let tree2 =  N(E, 1, N(E, 2, N(E, 3, E)));;

(*      1 *)
(*    2   3 *)
(*  4 *)
let tree3 =  N(N(N(E, 4, E), 2, E), 1, N(E, 3, E));;

(*      1 *)
(*    2   3 *)
(*  4   5 *)
let tree4 =  N(N(N(E, 4, E), 2, E), 1, N(N(E, 5, E), 3, E));;

let l = first_leaf tree4;;
let l2 = next_leaf l;;

(*Joel Bjornson*)

let (>>) f g = fun x -> g (f x)

let is_right (_, p) =
  match p with
  | (Right _)   -> true
  | _           -> false

let is_leaf (t,_) =
  match t with
  | E -> true
  | _ -> false

let is_leaf_node (t,_) =
  match t with
  | N(E,_,E)  -> true
  | _         -> false

let get_val (t,_) =
  match t with
  | N (_, v, _) -> v
  | _           -> raise Bottom

let most_left_c = 
  let rec aux c =
    try
      aux @@ move_left c
    with
    | _ ->
      c
  in
  aux >> move_up

let first_leaf_c =
  let rec aux c =
    if is_leaf c then
      raise Bottom
    else if is_leaf_node c then
      c
    else
      try
        aux @@ move_right c
      with
      | _ ->
        aux @@ move_up c
  in
  most_left_c >> aux

let next_leaf =
  let rec aux c =
    if is_right c then
      aux @@ move_up c
    else
      try
        first_leaf_c @@ move_right @@ move_up c
      with
      | _ ->
        aux @@ move_up c
  in
  aux

let fold_left f d =
  let rec aux s c =
    let s = f s (get_val c) in
    try
      aux s @@ next_leaf c
    with
    | _ ->
      s
  in
  first_leaf_c >> aux d

let most_left = of_tree >> most_left_c
let first_leaf  = of_tree >> first_leaf_c
let collect_leaves = of_tree >> fold_left (fun xs x -> x :: xs) [] >> List.rev

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
