module type LIST_UTILS = sig
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
  val slice : 'a list -> int -> int -> 'a list
  val range : int -> int -> int list
  val sum : int list -> int
end

module List_utils : LIST_UTILS = struct
  let rec take (k : int) (l : 'a list) : 'a list =
    match (k, l) with
    | n, _ when n <= 0 -> []
    | _, [] -> []
    | n, (x :: xs) -> x :: take (n - 1) xs

  let rec drop (k : int)  (l : 'a list) : 'a list =
    match (k, l) with
    | n, xs when n <= 0 -> xs
    | _, [] -> []
    | n, (_ :: xs) -> drop (n - 1) xs

  let slice (l : 'a list) (i : int) (j : int) = take (j - i) (drop i l)

  let range (s : int) (e : int) : int list =
    let rec loop acc s e =
      if s >= e then acc
      else loop (s :: acc) (s + 1) e 
    in List.rev (loop [] s e)

  let sum : int list -> int = List.fold_left (fun x y -> x + y) 0
end

open List_utils

let intervals (l : 'a list) =
  (*The set of intervals starting at position [i]*)
  let from (i : int) (l : 'a list) =
    let s  = slice l i (List.length l) in
    let f acc j = ((i, i + j - 1), sum (slice s 0 j)) :: acc
    in List.fold_left f [] (range 1 (List.length s + 1)) in
  (*The set of all intervals ([i = 0] to [i = List.length l - 1]*)
  List.rev (
      List.concat (
          List.fold_left 
            (fun acc i -> from i l :: acc) [] (range 0 (List.length l))
        )
    )

let sub_array_max_sum (l : int list) : ((int * int) * int) list =
  let t = intervals l in
  let m = List.fold_left (fun acc (_, s) -> max acc s) min_int t in
  List.fold_left  (fun acc (((_, _), z) as e) -> 
                   if z = m then e :: acc else acc) [] t

(*

  # sub_array_max_sum [1; 3; -8; 2; -1; 10; -2; 1] ;;
  - : ((int * int) * int) list = [((3, 5), 11)]

 *)
