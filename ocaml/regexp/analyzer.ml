type ('a, 'b) parsed =
| Returns of 'b * ('a list)
| Analyze_fails

type ('a, 'b) parser = 'a list -> ('a, 'b) parsed

exception Parse_error

let recognize_or_raise = function
  | Returns (v, []) -> v
  | _ -> raise Parse_error

let epsilon v toks = Returns (v, toks)

let token test =
  let f l =
    match l with
    | (t :: ts) -> 
      begin
        match test t with
        | Some r as v -> Returns (v, ts)
        | None -> Analyze_fails
           end
    | _ -> Analyze_fails in
  f

let (gives : ('a, 'b) parser -> ('b -> 'c) -> ('a, 'c) parser) =
  fun p f toks ->
    match p toks with
    | Returns (r, toks1) -> Returns (f r, toks1)
    | _ -> Analyze_fails

let parser_of_char c = token (fun c' -> if c = c' then Some c else None)

let parser_disjunction p1 p2 = fun toks ->
  match p1 toks with
  | Analyze_fails -> p2 toks
  | (_ as r) -> r

let parser_conjunction p1 p2 = 
  fun toks ->
  match p1 toks with
  | Returns (r1, toks1) ->
    begin
    match p2 toks1 with
    | Returns (r2, toks2) -> Returns ((r1, r2), toks2)
    | _ -> Analyze_fails
    end
  | _ -> Analyze_fails

let rec zero_or_more p =
  fun toks ->
    (parser_disjunction 
       (gives (parser_conjunction p (zero_or_more p)) (fun (x, xs) -> x :: xs )) 
       (epsilon ([]))) toks

