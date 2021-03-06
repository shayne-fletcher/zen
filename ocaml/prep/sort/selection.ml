let min l =
  let rec min_rec (i, m) j =
    if j >= List.length l then i
    else
      let h = List.nth l j in
      if h < m then
        min_rec (j, h) (j + 1)
      else min_rec (i, m) (j + 1)
  in
  min_rec (-1, max_int) 0

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

let rec selection_sort = function
  | [] -> []
  | l ->
    let j = min l in
    List.nth l j :: selection_sort (take l j @ drop l (j + 1))

let swap (arr : 'a array) (i : int) (j : int) =
  let t = arr. (j) in
  Array.set arr j (Array.get arr i);
  Array.set arr i t

let selection_sort' a =
  let n = Array.length a in
  for j = 0 to n - 1 do
    let min = ref j in
    for i = j + 1 to n - 1 do
      if a. (i) < a. (!min) then min := i;
    done;
    if !min <> j then swap a j !min
  done
