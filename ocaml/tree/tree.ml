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

let findmin (l : ('a tree * 'b) list) : 'a tree * 'b =
  let f acc elem =
    if snd elem < snd acc then elem else acc
  in List.fold_left f (List.hd l) (List.tl l)

let is_leaf = function | Leaf _ -> true | _ -> false

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

let string_of_code (sym, path) = Printf.sprintf "('%c', %s)" sym path

let () = Printf.printf "%s" (string_of_list string_of_code (encoding tree))
