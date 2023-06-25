let explode : string -> char list =
 fun s ->
  let n = String.length s in
  let rec loop acc i =
    if i = n then List.rev acc else loop (String.get s i :: acc) (i + 1)
  in
  loop [] 0

let implode : char list -> string =
 fun l ->
  let n = List.length l in
  let buf = Bytes.create n in
  let f i c = Bytes.set buf i c in
  List.iteri f l;
  Bytes.to_string buf

let bimap : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd =
 fun f g (x, y) -> (f x, g y)

let first : ('a -> 'b) -> 'a * 'c -> 'b * 'c = fun f -> bimap f Fun.id
let second : ('c -> 'd) -> 'a * 'c -> 'a * 'd = fun g -> bimap Fun.id g

let rec strip_prefix (pre : 'a list) (ys : 'a list) : 'a list option =
  match (pre, ys) with
  | [], ys -> Some ys
  | x :: xs, y :: ys when x == y -> strip_prefix xs ys
  | _, _ -> None

let rec strip_prefix_s (pre : string) (ys : string) : string option =
  Option.map implode (strip_prefix (explode pre) (explode ys))

let strip_suffix (a : 'a list) (b : 'a list) : 'a list option =
  Option.map List.rev (strip_prefix (List.rev a) (List.rev b))

let strip_suffix_s (a : string) (b : string) : string option =
  Option.map implode (strip_suffix (explode a) (explode b))

let rec strip_infix (needle : 'a list) (haystack : 'a list) :
    ('a list * 'a list) option =
  match (needle, haystack) with
  | needle, [] -> None
  | needle, x :: xs -> (
      match strip_prefix needle haystack with
      | Some rest -> Some ([], rest)
      | None -> Option.map (first (fun cs -> x :: cs)) (strip_infix needle xs))

(* e.g
   [strip_infix "::" "a::b::c" == Some ("a", "b::c")]
   [strip_infix "/" "foobar" == None]
*)
let strip_infix_s (needle : string) (haystack : string) :
    (string * string) option =
  Option.map
    (fun p -> bimap implode implode p)
    (strip_infix (explode needle) (explode haystack))

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
