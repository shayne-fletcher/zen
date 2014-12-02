type 'a remaining =
| Remains of 'a list
| Recognition_fails

type 'a recognizer = 'a list -> 'a remaining

let epsilon toks = Remains toks

let recognizer_of_token test = 
  fun toks -> 
    match toks with
  | (t :: ts) -> if test t then Remains ts else Recognition_fails
  | _ -> Recognition_fails

let recognizer_of_char c = recognizer_of_token (fun c' -> c = c')

let compose_or r1 r2 = fun toks ->
    match r1 toks with
    | (Remains _) as res -> res
    | _ -> r2 toks

let compose_and r1 r2 = fun toks ->
  match r1 toks with
  | Remains toks1 -> r2 toks1
  | _ -> Recognition_fails

let rec zero_or_more r = compose_or (compose_and r (zero_or_more r)) epsilon
