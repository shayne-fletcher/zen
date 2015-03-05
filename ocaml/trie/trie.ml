(**Lexical trees or 'tries' are used for the representation of
   dictionaries*)
module type TRIE = sig

    (*type trie = Letter of char * bool * (trie list)*)
    type t (* = trie*)
    (**The type of lexical trees*)

    val exists : t -> string -> bool
    (**Test if a word is in the dictionary*)

    val insert : t -> string -> t
    (**Take a dictionary and a word, return a new dictionary that
       additionally contains this word*)

    val construct : string list -> t
    (**Take a list of words and construct the corresponding
       dictionary*)

    val verify : t -> string list -> string list
    (**Takes a dictionary and a list of words and returns the list of
       words not in the dictionary*)

    val words : t -> string list
    (**Retrieve the list of words encoded in the dictionary*)

    val select : t -> int -> string list
    (**Retrieve the set of words in the dictionary of the given
      length*)

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

  (*'t must come first' principle:
    {{:http://blogs.janestreet.com/core-principles-uniformity-of-interface/}}*)

  type trie = Letter of char * bool * (trie list)
  type t = trie

  let exists (t : trie) (w : string) : bool = 
    let rec exists_aux ((Letter (d, e, nodes)) : trie) : (char list) -> bool = 
      function
      | [] -> false
      | [h] when e -> d = h
      | (h :: tl) -> d = h && List.exists (fun e -> exists_aux e tl) nodes in
    explode w |> exists_aux t

  let insert (t : trie) (w : string) : trie = 
    let rec insert_aux ((Letter (d, e, l) as n) : trie) : (char list) -> trie =
      function  
      | [] -> n
      | (x :: tl) -> 
         if x = d then insert_aux (Letter (d, tl = [], l)) tl
         else match List.partition (fun (Letter (e, _, _)) -> e = x) l with
           | [], _ -> Letter (d, e, (insert_aux (Letter (x, tl = [], [])) tl) :: l)
           | [h], m -> Letter (d, e, (insert_aux h tl :: m))
           | _ -> failwith "unexpected" in
    explode w |> insert_aux t

  let construct (words : string list) : trie =
    let k=String.get (List.nth words 0) 0 in
    let f (c, acc) w =
      if String.get w 0 = c then (k, insert acc w) else failwith "Mismatch" in
    snd @@ List.fold_left f (k, Letter (k, false, [])) words

  let verify (t : trie) (words : string list) : string list =
    List.fold_left (fun acc word -> if exists t word then acc else word :: acc) [] words

  let words (t : trie) : string list =
    let rec loop (path : char list) (acc : (char list) list) = function
      | Letter (d, terminal, cs) ->
         let path = d :: path in
         List.fold_left (loop path) 
            (if terminal then (path :: acc) else acc) cs
    in (loop [] [] t) |> List.map (fun s -> implode (List.rev s))

  let select (t : trie) (i : int) : string list =
    List.filter (fun w -> String.length w = i) (words t)

end

(*Test*)

let t = Trie.construct ["fa"; "false"; "far"; "fare"; "fried"; "frieze"]
let l = Trie.verify t ["far"; "fgar"; "fare"; "fared"]
let w2 = Trie.words t
let w3 = Trie.select t 5
