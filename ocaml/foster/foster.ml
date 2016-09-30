(*"A new top-down parsing algorithm to accomodate ambiguity and left
  recursion in polynomial time" -- 
  Richard A. Frost, ACM SIGPLAN Notices Vol.41, May 2006
*)

module Set_ops = struct
  let subtract (l : 'a list) (m : 'a list) : 'a list =
    let rec loop acc = function
      | [] -> List.rev acc
      | (h :: tl) -> loop (if List.mem h m then acc else h :: acc) tl in
    loop [] l

  let union (l : 'a list) (m : 'a list)  = 
    List.sort compare ((subtract l m) @ m)

  let rec union_concat  = 
    function | [] -> [] | (l :: r) -> union l (union_concat r)
end

type result = int list
type recognizer = char list -> int -> result

module Parse_ops = struct
  let explode : string -> char list =
    fun s ->
      let n = String.length s in
      let rec loop acc i =
        if i = n then List.rev acc
        else loop (String.get s i :: acc) (i + 1) in
      loop [] 0

  let implode : char list -> string =
    fun l ->
      let n = List.length l in
      let buf = Bytes.create n in
      let f i c = Bytes.set buf i c in
      List.iteri f l ; Bytes.to_string buf
end

let empty : recognizer = fun _ j -> [j]

let token (c : char) (toks : char list) (j : int) : result = 
  match j with
  | _ when j > (List.length toks - 1)  -> []
  | _ -> begin match List.nth toks j with
    | t  when t = c -> [(j + 1)]
    | _ -> []
  end

let ( |~ ) 
    (p : recognizer) 
    (q : recognizer) : char list -> int -> result =
  fun toks j -> Set_ops.union (p toks j)  (q toks j)

let ( &~ )
    (p : recognizer) 
    (q : recognizer) : char list -> int -> result =
  fun toks j -> 
    let r = p toks j in
    Set_ops.union_concat (List.map (fun i -> q toks i) r)

type memo_tbl_item_t = (int * result) list
type memo_tbl_t = (string, memo_tbl_item_t) Hashtbl.t

let memoize 
    (memo_tbl : memo_tbl_t)
    (tag : string) 
    (r : recognizer) : recognizer =
  fun toks j ->
    try
      let it : memo_tbl_item_t = Hashtbl.find memo_tbl tag in
      try
        let res = List.assoc j it in
        Printf.printf "Reusing result (%s, %d)\n" tag j;
        res
      with 
      | Not_found -> 
        let y = r toks j in
        Hashtbl.replace memo_tbl tag ((j, y) :: it);
        y    
    with
    | Not_found ->
      let y = r toks j in
      Hashtbl.add memo_tbl tag [(j, y)];
      y

let rec cache_fix 
    (memo_tbl : memo_tbl_t)
    (tag : string) 
    (f : recognizer -> recognizer)
    (toks : char list) 
    (j : int) : result =
  try
    let it : memo_tbl_item_t  = Hashtbl.find memo_tbl tag in
    try 
      let res = List.assoc j it in
      Printf.printf "Resusing result (%s, %d)\n" tag j;
      res
    with 
    | Not_found -> 
      let y = f (cache_fix memo_tbl tag f) toks j in
      Hashtbl.replace memo_tbl tag ((j, y) :: it);
      y    
  with | Not_found ->
    Hashtbl.add memo_tbl tag [];
    let y = f (cache_fix memo_tbl tag f) toks j in
    Hashtbl.replace memo_tbl tag ((j, y) :: (Hashtbl.find memo_tbl tag));
    y

let memo_tbl : memo_tbl_t = Hashtbl.create 16

(*
  s  := 's'
*)
let s : recognizer = memoize memo_tbl "s" (token 's')

(*
  ss := s ss ss | epsilon
*)
let ss : recognizer = 
  fun 
    (toks : char list) 
    (j : int) : result ->
  let ss_aux 
      (self : recognizer) 
      (toks : char list) 
      (j : int ) : result = 
    (s &~ self &~ self |~ empty) toks j in
  (cache_fix memo_tbl "ss" ss_aux) toks j

let parse 
    (p : recognizer) 
    (s : string) 
    (j : int) : (result * (string * memo_tbl_item_t) list) = 
  Hashtbl.clear memo_tbl;
  let res = List.map (( +) 1) (p (Parse_ops.explode s) (j - 1)) in
  let ls = Hashtbl.fold (fun t l acc -> (t, l) :: acc) memo_tbl [] in
  (res, ls)

let _ = parse ss "ssss" 1
