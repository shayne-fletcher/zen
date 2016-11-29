let string_of_list l = String.concat " " (List.map string_of_int l)

(*[fact n] computes the factorial of the natural number [n]*)
let rec factorial (n : int) : int = if n <= 1 then 1 else n * factorial (n - 1)

(*[digits n] computes a list of the base 10 digits of the natural
  number n*)
let digits n =
  let rec loop acc k =
    let k' = k / 10 in
    let rem = k mod 10 in
    let acc' = rem :: acc in
    if k' = 0 then acc' else loop acc' k'  in
  loop [] n

(* A factorion is a natural number that equals the sum of the
   factorials of its decimal digits. For example, 145 is a factorion
   because 1! + 4! + 5! = 1 + 24 + 120 = 145.
*)

(*[is_factorian n] returns [true] if [n] is a factorian otherwise [false]*)
let is_factorian n =
  List.fold_left (fun x y -> x + y) 0 (List.map factorial (digits n)) = n

(*[range start until] computes the list of numbers from [start] up-to
  but not including [until]*)
let range start until =
  let rec loop acc k =
    if k < start then acc 
    else loop (k :: acc) (k - 1) in
  loop [] (until - 1)

(*[insert n ns] adds [n] to [ns] if it isn't already there*)
let insert ns n = if List.mem n ns then ns else n :: ns

(*A number is said to be "pandigital" if it contains each of the
  digits from 0 to 9 (and whose leading digit must be nonzero)*)
let pandigital n =
  let (h :: ns) = digits n in
  let l = 
    List.sort 
      (Pervasives.compare) 
      (List.fold_left insert [] (h :: ns)) in
  h <> 0 && (l = range 0 10)

let gather ls =
  (*First, compute a list of all of the distinct elements in [ls]. For
    example, if [ls] is the list ['a'; 'a'; 'b'; 'b'; 'c'; 'a'; 'b';
    'a'], then the distinct elements form the list ['a'; 'b'; 'c'; 'd']*)
  let ds =  List.fold_left insert [] ls in
  (*Now, we consider each distinct element in turn and for each we
    compute a list by filtering out the occurences of that element from
    [ls] and add the resulting list to the list of lists [acc]*)
  let f acc d = (List.filter (fun x -> x = d) ls) :: acc in
  List.fold_left f [] ds

(*[partition p ls] computes two lists : a list of the elements in [ls]
  that satisfy [p] and a list of the elements in [ls] that don't*)
let partition p ls =
  let rec loop (xs, ys) = function
    | [] -> (xs, ys)
    | (h :: tl) -> loop (if p h then (h :: xs, ys) else (xs, h ::ys)) tl in
  loop ([], []) ls

(*[gather ls] produces a list of list of the distinct items of [ls]
  gathered together*)
let gather ls =
  let rec loop acc = function
   | [] -> acc
   | (h :: _) as s -> 
     let (xs, rem) = partition (fun x -> x = h) s in
     loop (xs :: acc) rem in
  loop [] ls
     
(*[bin_count ls] produces a list of the unique elements of [ls]
  together with a count of their occurences*)
let bin_count ls =
  let rec loop acc = function
    | [] -> acc
    | (h :: _) as s ->
      let (xs, rem) = partition (fun x -> x = h) s in
      loop ((h, (List.length xs)) :: acc) rem in
  loop [] ls
