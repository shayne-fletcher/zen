(**{b Insertion sort} comparison based sorting method with complexity
   {i O(n{^ 2})}*)
module type INSERTIONSORT = sig

    (**[take_while] applied to a predicate [p] and a list [xs],
       returns the longest prefix (possibly empty) of [xs] of elements
       that satisfy [p]*)
    val take_while : ('a -> bool) -> 'a list -> 'a list

    (**[take_while] applied to a predicate [p] and a list [xs],
       returns the longest prefix (possibly empty) of [xs] of elements
       that satisfy [p]. [drop_while] returns the remaining suffix*)
    val drop_while : ('a -> bool) -> 'a list -> 'a list

    (**[span p xs] is equivalent to [(take_while p xs, drop_while p
       xs)]*)
    val span : ('a -> bool) -> 'a list -> ('a list * 'a list)

    (**[insertion_sort] works by building the ordered list one element
       at a time*)
    val insertion_sort : 'a list -> 'a list

  end (*module type INSERTIONSORT*)

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

end (*module Insertion_sort*)

let unord=[3; 0; -1; 5; 2; 0]
let ordered = Insertion_sort.insertion_sort unord
