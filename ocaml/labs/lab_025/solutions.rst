(*Thomas Foster*)

let forbid_empty_selection = fun op c ->
    match (op c) with
    | (E, _) -> raise Bottom
    | _ as x -> x

let rec apply_until_raise f e a = try apply_until_raise f e (f a) with e -> a

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
