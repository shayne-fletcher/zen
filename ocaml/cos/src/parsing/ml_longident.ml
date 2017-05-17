type t =
| Lident of string
| Ldot of t * string
| Lapply of t * t

let flatten lid =
  let rec flat acc = function
    | Lident s -> s :: acc
    | Ldot (lid, s) -> flat (s :: acc) lid
    | Lapply (_, _) -> Ml_misc.fatal_error "Ml_longident.flat" 
  in
  flat [] lid

let last = function
  | Lident s -> s
  | Ldot (_, s) -> s
  | Lapply (_, _) -> Ml_misc.fatal_error "Ml_longident.last"

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let parse s =
  match split_at_dots s 0 with
  | [] -> Lident ""
  | hd :: tl -> List.fold_left (fun p s -> Ldot (p, s)) (Lident hd) tl
