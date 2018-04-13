(*[reverse l] computes a list from [l] where the elements are in
reverse order*)
let reverse (l : 'a list) : 'a list =
  let rec loop acc  = function
    | [] -> acc
    | (x :: xs) -> loop (x :: acc) xs in
  loop [] l

(*[swap_list i j l] computes a list from [l] where the elements at
positions [i] and [j] are swapped*)
let swap_list (i : int) (j : int) (l : 'a list) =
  let n = List.length l in
  if i < 0 || i >= n || j < 0 || j >= n  then
    failwith "swap"
  else
    let u = List.nth l i and v = List.nth l j in
    let rec loop acc k = function
      | [] -> List.rev acc
      | (x :: xs) ->
         loop
           ((if k = j then u else if k = i then v else x) :: acc)
           (k + 1) xs in
    loop [] 0 l

(*[swap_array i j arr] modifies [arr] by swapping the
elements at positions [i] and [j]*)
let swap_array (i : int) (j : int) (arr : 'a array) =
  let n = Array.length arr in
  if i < 0 || i >= n || j < 0 || j >= n  then
    failwith "swap_"
  else
    let t = Array.get arr j in
    Array.set arr j (Array.get arr i);
    Array.set arr i t;
    arr

(*[sorted s] is [true] if the elements of [s] are sorted in ascending
order and [false] otherwise*)
let rec sorted : int list -> bool = function
  | [] | [_] -> true
  | (x :: y :: tl) -> x <= y && sorted (y :: tl)

let lin_search (x : int) : int list -> int option =
  fun xs ->
    let rec loop i = function
      | [] -> None
      | (y :: ys) ->
         if x = y then Some i
         else loop (i + 1) ys
    in loop 0 xs

(*[bin_search x l] finds the index of [x] in the sorted list [l] by
binary search*)
let bin_search (x : int) : int list -> int option = function
  | [] -> None
  | xs ->
    let rec loop (l, u) =
      if l > u then None
      else
        let k = (l + u) / 2 in
        match compare x (List.nth xs k)  with
        |  0 -> Some k
        | -1 -> loop (0, k - 1)
        |  1 -> loop (k + 1, u)
        | _ -> assert false
    in  loop (0, List.length xs - 1)

exception Done

let bubble_sort (arr : int array) =
  try
    begin
      let n = ref (Array.length arr)in
      while true do
        let swapped =  ref false in
        for i = 1 to (!n - 1) do
          if Array.get arr (i - 1) > Array.get arr i then
            begin
              swap_array (i - 1) i arr;
              swapped := true
            end
        done;
        n := !n - 1;
        if not (!swapped) then (raise Done)
      done
    end
  with
  | Done -> ()

let rec insert n : int list -> int list = function
  | [] -> [n]
  | (x :: xs) as l -> if n < x then n :: l else x :: (insert n xs)

let rec insert_sort : int list -> int list = function
  | [] -> []
  | (x :: xs) -> insert x (insert_sort xs)

let rec drop l k =
  if k <= 0 then l
  else
    match l with
    | [] -> []
    | (_ :: tl) -> drop tl (k - 1)

let rec take l k =
  if k <= 0 then []
   else
     match l with
      | [] -> []
      | (h :: tl) -> h :: take tl (k - 1)

let rec range s e = if s > e - 1 then [] else s :: range (s + 1) e

let rec perms = function
  | [] -> [[]]
  | [x] -> [[x]]
  | _ as l->
    let f acc k =
      let e = List.nth l k in
      List.map (fun x -> e :: x) (perms @@ take l k @ drop l (k + 1)) @ acc in
    List.fold_left f [] (range 0 (List.length l))

let random_perms (n : int) : int list =
  let rec loop acc  =
    if List.length acc = n then acc
    else
      let k = (Random.int n) + 1 in
      if List.mem k acc then loop acc
      else loop (k :: acc) in
  loop []

let explode (s : string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1)(s.[i] :: l) in
  exp (String.length s - 1) []

let implode (l : char list) : string =
  let res : Bytes.t= Bytes.create (List.length l) in
  let rec imp i =
    fun l ->
      match l with
        | [] -> res
        | (c :: l) -> res.[i] <- c; imp (i + 1) l in imp 0 l

let rec prefix (us : 'a list) (vs : 'a list) : bool =
  match us, vs with
  | [], _ -> true
  | (u :: us), [] -> false
  | (u :: us), (v :: vs) -> u = v && prefix us vs

let endswith (us : 'a list) (vs : 'a list) : bool =  prefix (List.rev us) (List.rev vs)

let rec drop (t : 'a list) (k : int) : 'a list =
  if k <= 0 then t
  else
    if List.length t = 0 then
      []
    else drop (List.tl t) (k - 1)

let rec take (t : 'a list) (k : int) : 'a list =
  if k <= 0 then []
  else
    if List.length t = 0 then
      []
    else
      List.hd t :: take (List.tl t) (k - 1)

let substring (s : string) (xs : string) : int =
  let s' = explode s and xs' = explode xs in
  let rec loop i rem =
    match rem with
    | [] -> -1
    | (x :: xs) as l ->
       if prefix s' rem then i
       else
         loop (i + 1) xs in
  loop 0 xs'

let rec merge (l : int list) (m : int list) : int list =
  match (l, m) with
  | [], ys -> ys
  | xs, [] -> xs
  | (x :: xs), (y :: ys) ->
    if x <= y then x :: (merge xs (y :: ys))
    else y :: (merge (x :: xs) ys)

let rec merge_sort : int list -> int list = function
 | [] -> []
 | [_] as l -> l
 | xs as l ->
   let n = (List.length xs / 2) in
   merge (merge_sort (take l n)) (merge_sort (drop l n))


let take l k =
  let rec loop acc xs k =
    if k <= 0 || xs = [] then List.rev acc
    else
      match xs with
      | (h :: tl) -> loop (h :: acc) tl (k - 1) in
  loop [] l k
