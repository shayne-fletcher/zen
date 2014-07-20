module type INSERTIONSORT = sig

    val take_while : ('a -> bool) -> 'a list -> 'a list
    val drop_while : ('a -> bool) -> 'a list -> 'a list
    val span : ('a -> bool) -> 'a list -> ('a list * 'a list)
    val insertion_sort : 'a list -> 'a list

end

module Insertion_sort : INSERTIONSORT = struct

  let rec take_while p l = 
    match (p, l) with
    | p, [] -> []
    | p, (x :: xs) when p x -> x :: take_while p xs
    | _, _ -> []

  let rec drop_while p l = 
    match (p, l) with
    | p, [] -> []
    | p, (x :: xs) when p x -> drop_while p xs
    | _, xs -> xs

  let span p l = ((take_while p l), (drop_while p l))

  let insertion_sort l =
    let insert acc x =
      let l, r = span (fun y -> y < x) acc in
      l @ (x :: r)
    in
    List.fold_left insert [] l

end

let l = Insertion_sort.insertion_sort [3; 0; -1; 5; 2; 0]
