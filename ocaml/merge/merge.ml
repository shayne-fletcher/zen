(**{b Merge sort}, an {i O(n log n)} comparison based sorting
   algorithm *)
module type MERGESORT = sig

  (**[take n] applied to a list [xs], returns the prefix of [xs] of
     length [n] or [xs] itself if [n > List.length xs] e.g. [take 2
     [1; 2; 3]] {i = } [[1; 2]]*)
  val take : int -> 'a list -> 'a list

  (**[drop n] applied to a list [xs], returns the suffix of [xs] after
     the first [n] elements or, [[]] if [n > List.length xs]
     e.g. [drop 2 [1; 2; 3]] {i = } [[3]]*)
  val drop : int -> 'a list -> 'a list

  (**[split n xs] is equivalent to [take n xs, drop n xs] e.g. [split
     2 [1; 2; 3]] {i = } [([1; 2], [3])]*)
  val split : int -> 'a list -> 'a list * 'a list

  (**[merge] given two {b sorted} sequences [xs] and [ys] returns a
     single sorted sequence of the elements of [xs] and [ys]
     e.g. [merge [1; 3] [2; 4]] {i = } [[1; 2; 3; 4]]*)
  val merge : 'a list -> 'a list -> 'a list

  (**[merge_sort] works by splitting a sequence into two parts,
     recursively sorting the two parts and merging the results into a
     single sorted sequence e.g. [merge_sort [1; 2; -1; 0; 3]] {i = }
     [[-1; 0; 1; 2; 3]]*)
  val merge_sort : 'a list -> 'a list
end

module Merge_sort : MERGESORT = struct

  let rec take k l =
    match (k, l) with
    | n, _ when n <= 0 -> []
    | _, [] -> []
    | n, (x :: xs) -> x :: take (n - 1) xs

  let rec drop k l =
    match (k, l) with
    | n, xs when n <= 0 -> xs
    | _, [] -> []
    | n, (_ :: xs) -> drop (n - 1) xs

  let rec split k l = take k l, drop k l

  let rec merge l m =
    match (l, m) with
    | [], ys -> ys
    | xs, [] -> xs
    | ((x :: xs) as t), ((y :: ys) as s) -> 
      if x <= y then x :: (merge xs s) else y :: (merge t ys)
        
  let rec merge_sort l =
    let i = (List.length l) / 2 in
    if i = 0 then l
    else
      let u, v = split i l in
      let xs, ys = merge_sort u, merge_sort v in
      merge xs ys

end

(*Test*)

let l = Merge_sort.merge_sort [-1; 2; 0; 4; 3; 5]
