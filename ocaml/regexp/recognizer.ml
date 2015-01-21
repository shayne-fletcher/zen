type 'a remaining =
| Remains of 'a list
| Recognition_fails

type 'a recognizer = 'a list -> 'a remaining

let empty toks = Remains toks

let end_of_input = function
  | [] -> Remains []
  | _ -> Recognition_fails

let recognizer_of_token test = 
  fun toks -> 
    match toks with
  | (t :: ts) -> if test t then Remains ts else Recognition_fails
  | _ -> Recognition_fails

let recognizer_of_char c = recognizer_of_token (fun c' -> c = c')

let ( |~ ) r1 r2 = fun toks ->
    match r1 toks with
    | (Remains _) as res -> res
    | _ -> r2 toks

let ( &~ ) r1 r2 = fun toks ->
  match r1 toks with
  | Remains toks1 -> r2 toks1
  | _ -> Recognition_fails

let compose_or r rl = List.fold_right ( |~ ) rl r

let compose_and rl = List.fold_left ( &~ ) empty rl

let rec zero_or_more r = fun toks -> ((r &~ zero_or_more r) |~ empty) toks

let rec one_or_more r = fun toks -> (r &~ (zero_or_more r)) toks
