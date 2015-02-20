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
  List.map (function |'0' -> L|'1' -> R | _ -> failwith "invalid") (explode s)

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

let collect_leaves l =
  let f acc e =
    match e with
    | (Leaf _, p) -> e :: acc
    | _ -> acc
  in List.fold_left f [] l
    
let findmin (l : ('a tree * 'b) list) : 'a tree * 'b =
  let leaves = collect_leaves l in
  let f acc elem =
    if snd elem < snd acc then elem else acc
  in List.fold_left f (List.hd l) (List.tl l)

let huffman (l : ('a * float) list) : 'a tree =
  let f x = let (c, p) = x in Leaf c, p in
  let l = List.map f l in
  let rec loop acc =
    match acc with
    | [] -> failwith "empty"
    | [(Node (left, right), p) as n] -> acc
    | (h :: tl) ->
        let m = findmin acc in
        let acc = List.filter (fun e -> e <> m)  acc in
        let n = findmin acc in
        let acc = List.filter (fun e -> e <> n)  acc in
        let pair = Node (fst m, fst n), snd m +. snd n in
        loop (pair :: acc) 
  in fst (List.hd (loop l))

let tree = huffman ['a', 0.4; 'b', 0.35; 'c', 0.2; 'd', 0.05]

(* let l = decode (direction_list_of_string "0000010100111") t *)

