let explode : string -> char list =
 fun s ->
  let n = Stdlib.String.length s in
  let rec loop acc i =
    if i = n then List.rev acc else loop (Stdlib.String.get s i :: acc) (i + 1)
  in
  loop [] 0

let implode : char list -> string =
 fun l ->
  let n = Stdlib.List.length l in
  let buf = Stdlib.Bytes.create n in
  let f i c = Stdlib.Bytes.set buf i c in
  Stdlib.List.iteri f l;
  Stdlib.Bytes.to_string buf

let bimap : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd =
 fun f g (x, y) -> (f x, g y)

let first : ('a -> 'b) -> 'a * 'c -> 'b * 'c = fun f -> bimap f Fun.id
let second : ('c -> 'd) -> 'a * 'c -> 'a * 'd = fun g -> bimap Fun.id g

let rec strip_prefix (pre : 'a list) (ys : 'a list) : 'a list option =
  match (pre, ys) with
  | [], ys -> Some ys
  | x :: xs, y :: ys when x == y -> strip_prefix xs ys
  | _, _ -> None

let strip_suffix (a : 'a list) (b : 'a list) : 'a list option =
  Option.map Stdlib.List.rev
    (strip_prefix (Stdlib.List.rev a) (Stdlib.List.rev b))

let rec strip_infix (needle : 'a list) (haystack : 'a list) :
    ('a list * 'a list) option =
  match (needle, haystack) with
  | needle, [] -> None
  | needle, x :: xs -> (
      match strip_prefix needle haystack with
      | Some rest -> Some ([], rest)
      | None -> Option.map (first (fun cs -> x :: cs)) (strip_infix needle xs))

(*
-- | Replace a subsequence everywhere it occurs.
--
-- > replace "el" "_" "Hello Bella" == "H_lo B_la"
-- > replace "el" "e" "Hello"       == "Helo"
-- > replace "" "x" "Hello"         == "xHxexlxlxox"
-- > replace "" "x" ""              == "x"
-- > \xs ys -> replace xs xs ys == ys
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] to xs = go xs
    where go [] = to
          go (x:xs) = to ++ x : go xs
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []
*)
