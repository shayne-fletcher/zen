(*Numbers less than [n] that are multiples of the numbers in [l]*)
let multiples_of (l : int list) (n : int) : int list=
  let rec loop (acc : int list) : int -> int list = function
    | t when t = n -> acc
    | i -> loop (if List.fold_left 
        (fun acc x -> acc || i mod x = 0) false l
      then i :: acc else acc) (i + 1) in
  loop [] 1
(*Sum of the numbers less than 1000 that are multiples of 3 or 5*)
let sum :int = List.fold_left (fun x y -> x + y) 0 (multiples_of [3; 5] 1000)
