(**Lexical trees or 'tries' are used for the reprsentation of
   dictionaries*)
module type TRIE = sig
    (* type trie = Letter of char * bool * (trie list) *)
    type t(* = trie*)
    (**The type of lexical trees*)

    val exists : string -> t -> bool
    (**Test if a word is in the dictionary*)

    val insert : string -> t -> t
    (**Take a word and a dictionary and compute a new dictionary that
       additionally contains this word*)

    val construct : string list -> t
    (**Take a list of words and construct the corresponding
       dictionary*)

    val verify : string list -> t -> string list
    (**Takes a list of words and a dictionary and returns the list of
       words not in the dictionary*)

    val words : t -> string list
    (**Retrieve the list of words encoded in the dictionary*)

    val select : t -> int -> string list
    (**Retrieve the set of words in the dictionary of the given length*)
end

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

module Trie : TRIE = struct

  type trie = Letter of char * bool * (trie list)
  type t = trie

  let exists (w : string) (t : trie) : bool = 
    let rec exists_aux (cs : char list) ((Letter (d, e, nodes)) : trie) : bool =
      match cs with
      | [] -> false
      | [h] when e -> d = h
      | (h :: tl) -> d = h && List.exists (fun e -> exists_aux tl e) nodes in
    exists_aux (explode w) t

  let insert (w : string) (t : trie) : trie = 
    let rec insert_aux cs (Letter (d, e, l) as n) =
      match cs with
      | [] -> n
      | (x :: tl) -> 
         if x = d then
           insert_aux tl (Letter (d, List.length tl = 0, l))
         else
           match List.partition (fun (Letter (e, _, _)) -> e = x) l with
           | [], _ -> Letter (d, e, (insert_aux tl (Letter (x, List.length tl = 0, []))) :: l)
           | [h], m -> Letter (d, e, (insert_aux tl h :: m))
           | _ -> failwith "unexpected" in
    insert_aux (explode w) t

  let construct (words : string list) : trie =
    let k=String.get (List.nth words 0) 0 in
    let f (c, acc) w =
      if String.get w 0 = c then (k, insert w acc) else failwith "Mismatch" in
    snd (List.fold_left f (k, Letter (k, false, [])) words)

  let verify (words : string list) (t : trie) : string list =
    let f acc word = if exists word t then acc else word :: acc in
    List.fold_left f [] words

  let words (t : trie) : string list =
    let rec loop path acc node=
      begin
        match node with
        | Letter (d, false, child) ->
           let path = d :: path in List.fold_left (loop path) acc child
        | Letter (d, true, child) ->
           let path = d :: path in List.fold_left (loop path) (path :: acc) child
      end
    in List.map (fun s -> implode (List.rev s)) (loop[] [] t)

  let select (t : trie) (i : int) : string list =
    List.filter (fun w -> String.length w = i) (words t)

end

(*Test*)

let t = Trie.construct ["fa"; "false"; "far"; "fare"; "fried"; "frieze"]
let l = Trie.verify ["far"; "fgar"; "fare"; "fared"] t
let w2 = Trie.words t
let w3 = Trie.select t 5
