(*"A new top-down parsing algorithm to accomodate ambiguity and left
  recursion in polynomial time" -- 
  Richard A. Frost, ACM SIGPLAN Notices Vol.41, May 2006
*)

module Set_ops = struct
  (*[subtract l m] returns the list [l] where all elements structurally
    equal to one in [m] have been removed*)
  let subtract (l : 'a list) (m : 'a list) : 'a list =
    let rec loop acc = function
      | [] -> List.rev acc
      | (h :: tl) -> loop (if List.mem h m then acc else h :: acc) tl in
    loop [] l

  (*[union l m] appends before [m] all the elements of [l] that are not
    structurally equal to an element of [m]*)
  let union (l : 'a list) (m : 'a list)  = 
    List.sort compare ((subtract l m) @ m)

  (*[union_concat sets] computes the union of a set of sets*)
  let rec union_concat  = 
    function | [] -> [] | (l :: r) -> union l (union_concat r)
end

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

(*Parser combinators*)

(*The type of the result of a parse*)
type result = int list

(*The type of recognizers*)
type recognizer = char list -> int -> result

(*[empty toks j] always returns the set {j}*)
let empty : recognizer = fun _ j -> [j]

(*[token c toks j] returns the set {j + 1} if [j] is a valid index and
  the character at that index in [toks] is [c] else the empty set *)
let token (c : char) (toks : char list) (j : int) : result = 
  match j with
  | _ when j > (List.length toks - 1)  -> []
  | _ -> begin match List.nth toks j with
    | t  when t = c -> [(j + 1)]
    | _ -> []
  end

(*[p |~ q] computes the disjunction of the two parsers [p] and [q]
  defined as the union of the results of parsing with [p] and parsing
  with [q]*)
let ( |~ ) 
    (p : recognizer) 
    (q : recognizer) : char list -> int -> result =
  fun toks j -> Set_ops.union (p toks j)  (q toks j)

(*[ p &~ q] computes the conjunction of the two parsers [p] and [q]
  defined as the union of the results of applying [q] to all the results
  of [p]*)
let ( &~ )
    (p : recognizer) 
    (q : recognizer) : char list -> int -> result =
  fun toks j -> 
    let r = p toks j in
    Set_ops.union_concat (List.map (fun i -> q toks i) r)

(*Parser memoization*)

(*The type of a memoization table entry. An association list, an int
  representing an index together with the result of a parse on that
  index*)
type memo_tbl_item_t = (int * result) list

(*The type of the memoization table. A hash table from strings to an
  association list*)
type memo_tbl_t = (string, memo_tbl_item_t) Hashtbl.t

open Format

(*[line i f s] formats whitespace on [pp] proportional to the depth
  indicator [i] before computing the format operation indicated by
  [s]*)
let line 
    (i : int) 
    (ppf : formatter) 
    (s : ('a, formatter, unit) format) : 'a =
  fprintf ppf "%s" (String.make ((2 * i) mod 72) ' ');
  fprintf ppf s

(*[pair i f ppf p] formats a pair on [ppf] with a depth indicator
  given by [i] by way of [f]*)
let pair 
    (i : int)
    (f : int -> formatter -> 'a -> 'b)
    (ppf:formatter) 
    ((u, v) : 'a * 'a ) : unit =
    line i ppf "(\n";
    f (i + 1) ppf u;
    f (i + 1) ppf v;
    line i ppf ")\n"

(*[list i f ppf p] formats a list on [ppf] with a depth indicator
  given by [i] by way of [f]*)
let list 
    (i : int)
    (f : int -> formatter -> 'a -> unit)
    (ppf : formatter)
    (l : 'a list) : unit =
  match l with
  | [] -> line i ppf "[]\n";
  | _ :: _ ->
     line i ppf "[\n";
     List.iter (f (i + 1) ppf) l;
     line i ppf "]\n"

(*Format functions for pretty printing parse results*)
let print_memo_entry 
    (i : int) 
    (ppf : formatter) 
    (res : int * int list) : unit =
  let f ppf r = 
    fprintf ppf "%d -> [%s]" (fst res) 
      (String.concat "; " (List.map string_of_int (snd res))) in
  line i ppf "%a\n" f res
and print_memo_tbl_binding 
    (i : int) 
    (ppf : formatter) 
    (entry : (string * memo_tbl_item_t)) : unit =
  let (tag, items) = entry in
  line i ppf "%s\n" tag;
  list (i + 1) print_memo_entry ppf items
and print_memo_tbl 
    (i : int) 
    (ppf : formatter) 
    (t : memo_tbl_t) : unit =
  line i ppf "memo_table\n";
  list 
    (i + 1) 
    print_memo_tbl_binding 
    ppf 
    (Hashtbl.fold (fun t l acc -> (t, l) :: acc) t [])
and print_parse_result
    (i : int)
    (ppf : formatter)
    (res : result) : unit = 
  line i ppf "result\n";
  line 
    (i + 1) 
    ppf "%a\n"
    (fun ppf l -> 
      fprintf ppf "[%s]" (String.concat "; " (List.map string_of_int l)))
    res
and print_parse
    (i : int) 
    (ppf : formatter) 
    (res : (result * memo_tbl_t)) : unit = 
  line i ppf "parse\n";
  print_parse_result (i + 1) ppf (fst res);
  print_memo_tbl (i + 1) ppf (snd res)

let string_of_parse (res : (result * memo_tbl_t)) : string =
  print_parse 0 (str_formatter) res;
  flush_str_formatter ()

(*Forward declaration*)
let global_memo_tbl : (unit -> memo_tbl_t) ref = ref (fun () -> assert false)

(*The [cache] operator is for non-recursive rules*)
let cache (tag : string) (r : recognizer) : recognizer =
  fun (toks : char list) (j : int) : result ->
    let memo_tbl = !global_memo_tbl () in
    try
      let it : memo_tbl_item_t = 
        Hashtbl.find memo_tbl tag in
      try
        let res = List.assoc j it in
        (* Printf.printf "Reusing result (%s, %d)\n" tag j; *)
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

(*The [cache_fix] operator is for recursive rules*)
let rec cache_fix 
    (tag : string) 
    (f : recognizer -> recognizer) = 
  fun (toks : char list) (j : int) : result ->
    let memo_tbl = !global_memo_tbl () in
    try
      let it : memo_tbl_item_t = 
        Hashtbl.find memo_tbl tag in
      try 
        let res = List.assoc j it in
        (* Printf.printf "Resusing result (%s, %d)\n" tag j; *)
        res
      with 
      | Not_found -> 
        let y = f (cache_fix tag f) toks j in
        Hashtbl.replace memo_tbl tag ((j, y) :: it);
        y    
    with 
    | Not_found ->
      (* Printf.printf "Creating entry for %s\n" tag; *)
      Hashtbl.add memo_tbl tag [];
      let y = f (cache_fix tag f) toks j in
      (* Printf.printf "Adding entry for %s, %d\n" tag j; *)
      Hashtbl.replace memo_tbl tag ((j, y) :: Hashtbl.find memo_tbl tag);
      y

(*Testing*)

(*
  s  := 's'
*)
let s : recognizer = cache "s" (token 's')

(*
  ss := s ss ss | epsilon
*)
let ss : recognizer = fun (toks : char list) (j : int) : result ->
  let ss_aux self toks j = 
    (s &~ self &~ self |~ empty) toks j in
  (cache_fix "ss" ss_aux) toks j

let parse 
    (p : recognizer) 
    (s : string) 
    (j : int) : (result * memo_tbl_t) = 
  let memo_tbl = Hashtbl.create 16 in
  let () = global_memo_tbl := (fun () -> memo_tbl) in
  let res = List.map (( + ) 1) (p (Parse_ops.explode s) (j - 1)) in
  (res, memo_tbl)

let results = parse ss "ssss" 1 |> string_of_parse |> print_endline
