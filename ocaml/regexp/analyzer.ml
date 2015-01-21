type ('a, 'b) parsed =
| Returns of 'b * 'a list
| Analyze_fails

type ('a, 'b) parser = 'a list -> ('a, 'b) parsed

exception Parse_error of string

let (accept : ('a, 'b) parsed -> 'b) = function
   | Returns (v, []) -> v
   | Returns (_, _ :: _) -> raise (Parse_error "Input remaining")
   | Analyze_fails  ->  raise (Parse_error "Parse failure")

let (empty : 'b -> ('a, 'b) parser) = fun v toks -> Returns (v, toks)

let (token : ('a -> 'b option) -> ('a, 'b) parser) =
  fun test ->
    let f l =
      match l with
      | (t :: ts) -> 
         begin
           match test t with
           | Some r -> Returns (r, ts)
           | None -> Analyze_fails
         end
      | _ -> Analyze_fails in
    f

let (char : 'a -> ('a, 'a) parser) = 
  fun c -> token (fun c' -> if c = c' then Some c else None)

(*gives*)
let (( |>~ ) : ('a, 'b) parser -> ('b -> 'c) -> ('a, 'c) parser) =
  fun p f toks ->
    match p toks with
    | Returns (r1, toks1) -> Returns (f r1, toks1)
    | Analyze_fails -> Analyze_fails

(*orelse*)
let (( |~ ) : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser) =
  fun p1 p2 toks ->
    match p1 toks with
    | Analyze_fails -> p2 toks
    | res -> res

(*andalso*)
let (( &~ ) : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'b * 'c) parser) =
  fun p1 p2 toks ->
    match p1 toks with
    | Returns (r1, toks1) -> 
       (match p2 toks1 with
        | Returns (r2, toks2) -> Returns ((r1, r2), toks2)
        | _ -> Analyze_fails)
    | _ -> Analyze_fails

let rec (zero_or_more  : ('a, 'b) parser -> ('a, 'b list) parser) =
  fun p toks -> 
    (((p &~ (zero_or_more p)) |>~ (fun (x, xs) -> x :: xs)) |~ (empty [])) toks
