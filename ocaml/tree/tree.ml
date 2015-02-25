let string_of_list f l = 
  "[" ^ String.concat ";" (List.map f l) ^ "]" ;;

let explode s =
  let n = String.length s in
  let rec loop acc i =
    if i = n then List.rev acc
    else loop (String.get s i :: acc) (i + 1) in
  loop [] 0

let implode l =
  let n = List.length l in
  let buf = Bytes.create n in
  let f i c = Bytes.set buf i c in
  List.iteri f l ; Bytes.to_string buf

type 'a tree = | Leaf of 'a | Node of 'a tree * 'a tree

type direction = | L | R

let direction_list_of_string s =
  List.map (function |'0' -> L| '1' -> R | _ -> failwith "invalid") (explode s)

let string_of_direction_list l = implode (List.map (function | L -> '0' | R -> '1') l)

let rec subtree (l : direction list) (t : 'a tree) : 'a tree option =
  match l with
  | [] -> Some t
  | (h :: tl) -> 
    match t with
    | Leaf _ -> None
    | Node (left, right) -> 
      if h = L then
        subtree tl left
      else 
        subtree tl right

let decode (l : direction list) (t : 'a tree) : 'a list option = 
  let rec aux l t' acc =
  match l with
  | [] -> Some (List.rev acc)
  | (h :: tl) ->
    begin
      let s = subtree [h] t' in
      match s with
      | Some (Leaf x) -> aux tl t (x :: acc)
      | Some (Node (l, r) as n) -> aux tl n acc
      | _ -> None
    end
  in aux l t []

let is_leaf = function | Leaf _ -> true | _ -> false

let findmin (l : ('a tree * 'b) list) : 'a tree * 'b =
  let f acc elem =
    if snd elem < snd acc then elem else acc
  in List.fold_left f (List.hd l) (List.tl l)

let huffman (l : ('a * float) list) : 'a tree =
  let rec loop acc =
    match acc with
    | [] -> failwith "empty"
    | [(Node (left, right), p)] -> acc
    | (h :: tl) ->
        let m = findmin acc in
        let acc = List.filter (fun e -> e <> m)  acc in
        let n = findmin acc in
        let acc = List.filter (fun e -> e <> n)  acc in
        loop ((Node (fst m, fst n), snd m +. snd n) :: acc)
  in fst (List.hd (loop (List.map (fun (c, p) -> Leaf c, p) l)))

let tree = huffman ['a', 0.4; 'b', 0.35; 'c', 0.2; 'd', 0.05]

let encoding (t : 'a tree) : ('a * string) list =
  let rec loop (node : 'a tree) (path : direction list) (acc : ('a * direction list) list) =
    match node with
    | Leaf x -> (x, List.rev path) :: acc
    | Node (left, right) -> loop right (R :: path) (loop left (L :: path) acc)
  in List.map (fun (s, p) -> (s, string_of_direction_list p)) (loop t [] [])

let string_of_code (sym, path) = Printf.sprintf "('%c', \"%s\")" sym path

let () = Printf.printf "%s\n" (string_of_list string_of_code (encoding tree))

type ('a, 'b) parsed =
| Returns of 'b * 'a list
| Analyze_fails

type ('a, 'b) parser = 'a list -> ('a, 'b) parsed

let (accept : ('a, 'b) parsed -> 'b) = function
   | Returns (v, []) -> v
   | Returns (_, _ :: _) -> failwith "Couldn't consume all input"
   | Analyze_fails  -> failwith "Failed"

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

let (( |>~ ) : ('a, 'b) parser -> ('b -> 'c) -> ('a, 'c) parser) =
  fun p f toks ->
    match p toks with
    | Returns (r1, toks1) -> Returns (f r1, toks1)
    | Analyze_fails -> Analyze_fails

let (( |~ ) : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser) =
  fun p1 p2 toks ->
    match p1 toks with
    | Analyze_fails -> p2 toks
    | res -> res

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

let rec (char_range : char -> (char * char) list -> bool)= 
  fun c -> function
        | [] -> false
        | ((c1, c2) :: l) -> 
           (int_of_char c1 <= int_of_char c && 
              int_of_char c <= int_of_char c2) 
           || char_range c l

type token =
| T_char of char
| T_comma | T_lparen | T_rparen 

let (paren : (char, token) parser) = token (function | '(' -> Some T_lparen | ')' -> Some T_rparen | _ -> None)
let (comma_ : (char, token) parser) = token (function | ',' -> Some T_comma | _ -> None)
let (space : (char, unit) parser) = token (function | ' '| '\t' | '\r' | '\n' -> Some () | _ -> None)
let rec (spaces : (char, unit) parser)= fun toks -> (((space &~ spaces) |>~ (fun _ -> ())) |~ empty ()) toks
let (letter : (char, token) parser) = token (fun c -> if char_range c [('a', 'z'); ('A', 'Z')] then Some (T_char c) else None)
(* 
  lex := spaces((paren|comma|letter)spaces)*
*)
let (lex : (char, token list) parser) = spaces &~ (zero_or_more (((paren |~ comma_ |~ letter) &~ spaces) |>~ (fun (tok, ()) -> tok))) |>~ fun ((), toks) -> toks
(*
  tree :=
    | leaf
    | (tree, tree)
*)
let open_paren : (token, unit) parser = token (function | T_lparen -> Some () | _ -> None)
let close_paren : (token, unit) parser = token (function | T_rparen -> Some () | _ -> None)
let comma : (token, unit) parser = token (function | T_comma -> Some () | _ -> None)
let rec (leaf : (token, char tree) parser) =
  token (function | T_char c -> Some (Leaf c) | _ -> None)
and (tree : (token, char tree) parser) =
  fun toks -> (
      leaf
      |~ ((open_paren &~ tree &~ comma &~ tree &~ close_paren) 
                 |>~ (fun ((((_, l),_), r), _) -> Node (l, r)))
  ) toks
let tokenize : string -> token list = fun s -> s |> explode |> lex |> accept
let tree_of_string : string -> char tree = fun s -> s |> tokenize |> tree |> accept

let t = tree_of_string "(((a, b), (c, d)), e)"
