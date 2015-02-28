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

type trie = Letter of char * bool * (trie list)

let rec exists_aux (cs : char list) ((Letter (d, e, nodes)) : trie) : bool =
  match cs with
  | [] -> false
  | [h] when e -> d = h
  | (h :: tl) -> d = h && List.exists (fun e -> exists_aux tl e) nodes

let exists (w : string) (t : trie) : bool = exists_aux (explode w) t

let rec insert_aux cs (Letter (d, e, l) as n) =
  if exists_aux cs n then n
  else match cs with
  | [] -> n
  | (x :: []) -> Letter (d, e, (Letter (x, true, []) :: l))
  | (x :: y :: tl) ->
    if x = d then
      match List.partition (fun (Letter (e, _, _)) -> e = y) l with
      | [], _ -> Letter (d, e, (insert_aux tl (Letter (y, (List.length tl = 0), []))) :: l)
      | [h], m -> Letter (d, e || (List.length tl = 0), (insert_aux tl h) :: m)
      | _ -> failwith "unexpected"
    else 
      let () = Printf.printf "Here is an %c where x = %c \n" y x
      in Letter (d, e, (insert_aux (y :: tl) (Letter (x, false, []))))

let insert (w : string) (t : trie) = insert_aux (explode w) t

let t = Letter ('f', false, [Letter ('a', true, [Letter ('r', true, [])])])
let test_0 = exists "far" t
let test_1 = exists "fark" t
let test_2 = exists "f" t
let test_3 = exists "fa" t

let root = Letter ('f', false, [])

let t = insert "fare" (insert "far" root)


(* let t = insert "frieze" (insert "fried" (insert "fare" (insert "far" (insert "false" (insert "fa" (insert "fa" root)))))) *)
