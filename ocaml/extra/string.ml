open List

let rec strip_prefix_s (pre : string) (ys : string) : string option =
  Option.map implode (strip_prefix (explode pre) (explode ys))

let strip_suffix_s (a : string) (b : string) : string option =
  Option.map implode (strip_suffix (explode a) (explode b))

(* e.g
   [strip_infix "::" "a::b::c" == Some ("a", "b::c")]
   [strip_infix "/" "foobar" == None]
*)
let strip_infix_s (needle : string) (haystack : string) :
    (string * string) option =
  Option.map
    (fun p -> bimap implode implode p)
    (strip_infix (explode needle) (explode haystack))
