let rec quicksort = function
  | []| [_] as x -> x
  | h :: tl ->
    let left, right =
      List.fold_left (fun (l, r) x ->
          if x <= h then (x :: l, r)  else (l, x :: r)
        ) ([], []) tl in
    quicksort left @ [h] @ quicksort right
