let rec take ls k =
  if k <= 0 then []
  else match ls with
    | [] -> []
    | h :: tl -> h :: take tl (k - 1)

let rec drop ls k =
  if k <= 0 then ls
  else match ls with
    | [] -> []
    | h :: tl -> drop tl (k - 1)

let rec merge xs ys = match (xs, ys) with
  | xs, [] -> xs
  | [], ys -> ys
  | x :: xs, y :: ys ->
    if x < y then x :: merge xs (y :: ys) else y :: merge (x :: xs) ys

let rec merge_sort xs =
  match xs with
  | [] | [_] as x -> x
  | _ -> let k = List.length xs / 2 in
    merge (merge_sort (take xs k)) (merge_sort (drop xs k))
