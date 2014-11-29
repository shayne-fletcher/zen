(*val string_of_regexp : Syntax.regular_expression -> string*)

let rec string_of_regexp re =
  match re with
  | Syntax.Epsilon -> "Epsilon"
  | Syntax.Character c -> Printf.sprintf "Character '%c'" (Char.chr c)
  | Syntax.Sequence (p, q) -> Printf.sprintf "Sequence (%s, %s)" (string_of_regexp p) (string_of_regexp q)
  | Syntax.Alternative (p, q) -> Printf.sprintf "Alternative (%s, %s)" (string_of_regexp p) (string_of_regexp q)
  | Syntax.Repetition r -> Printf.sprintf "Repetition (%s)" (string_of_regexp r)

(*val regexp_of_string : string -> Syntax.regular_expression*)

let regexp_of_string s =
  let parse_buf lexbuf =
    try 
      Parser.main Lexer.main lexbuf
    with 
    | Parsing.Parse_error ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        raise 
          (Failure
             (Printf.sprintf 
                "file \"<string>\", line %d, character %d\n\
Error : Syntax error \"%s\"" line cnum tok
             )
          )
      end
  in parse_buf (Lexing.from_string s)

module Int_set : Set.S with type elt = int = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = int
  end)

type augmented_regexp =
  | Epsilon
  | Character of char * int
  | Sequence of augmented_regexp * augmented_regexp * pos
  | Alternative of augmented_regexp * augmented_regexp * pos
  | Repetition of augmented_regexp * pos
  | Accept of int
and pos = {
  null:bool;
  first:Int_set.t;
  last:Int_set.t;
}

let reset_label, generate_label =
 let r = ref (-1) in
 ((fun () -> r := (-1)), (fun () -> r := !r + 1; !r))

(*val null_pos : augmented_regexp -> bool*)

let null_pos x =
  match x with
  | Epsilon -> true
  | Character (_, i) -> false
  | Sequence (_, _, p) -> p.null
  | Alternative (_, _, p) -> p.null
  | Repetition (_, p) -> p.null
  | Accept _ -> false

(*val first_pos : augmented_regexp -> Int_set.t*)

let first_pos x =
  match x with
  | Epsilon -> Int_set.empty
  | Character (_, i) -> Int_set.add i (Int_set.empty)
  | Alternative (_, _, p) -> p.first
  | Repetition (_, p) -> p.first
  | Sequence (_, _, p) -> p.first
  | Accept i -> Int_set.add i (Int_set.empty)

(*val last_pos : augmented_regexp -> Int_set.t*)

let last_pos x =
  match x with
  | Epsilon -> Int_set.empty
  | Character (_, i) -> Int_set.add i (Int_set.empty)
  | Alternative (_, _, p) -> p.last
  | Repetition (_, p) -> p.last
  | Sequence (_, _, p) -> p.last
  | Accept i -> Int_set.add i (Int_set.empty)

(*val epsilon : unit -> augmented_regexp*)

let epsilon () = 
  Epsilon

(*val character : char -> augmented_regexp*)

and character c = 
  Character (c, generate_label ())

(*val repetition : augmented_regexp -> augmented_regexp*)

and repetition e = 
  Repetition (e, {null=true;first=first_pos e; last=last_pos e})

(*val alternative : augmented_regexp -> augmented_regexp -> augmented_regexp*)
    
and alternative e1 e2 = 
  Alternative (e1, e2, 
               {null=null_pos e1 || null_pos e2;
                first=Int_set.union (first_pos e1)(first_pos e2); 
                last=Int_set.union (last_pos e1) (last_pos e2)})

(*val sequence : augmented_regexp -> augmented_regexp -> augmented_regexp*)

and sequence e1 e2 = 
  let b1 = null_pos e1 
  and b2 = null_pos e2 in
  Sequence (e1, e2, 
            {null=b1 && b2;
             first=
                if b1 then Int_set.union (first_pos e1)(first_pos e2)
                else (first_pos e1); 
             last=
                if b2 then Int_set.union (last_pos e1) (last_pos e2)
                else last_pos e2})

(*val accept : augmented_regexp -> augmented_regexp*)

let accept (e:augmented_regexp) = 
  sequence e (Accept (generate_label ()))

(*val augmented_regexp : Syntax.regular_expression -> augmented_regexp*)

let rec augmented_regexp (x:Syntax.regular_expression) =
  match x with
  | Syntax.Epsilon -> epsilon ()
  | Syntax.Character i ->  character (Char.chr i)
  | Syntax.Sequence (x, y) -> 
    (*Be very careful here. Evaluation order matters!*)
    let x' = (augmented_regexp x)
    and y' = (augmented_regexp y) in
    sequence x' y'
  | Syntax.Alternative (x, y) -> 
    (*Be very careful here. Evaluation order matters!*)
    let x' = (augmented_regexp x)
    and y' = (augmented_regexp y) in
    alternative x' y'
  | Syntax.Repetition x -> repetition (augmented_regexp x)

(*val parse_augmented_regexp : string -> augmented_regexp * int*)

let parse_augmented_regexp s =
  let () = reset_label () in
  let ast = regexp_of_string s in
  let re1 = augmented_regexp ast in
  let re2 = accept  re1 in
  let count = generate_label () in
  (re2, count)

(*val string_of_set : (Int_set.elt -> string) -> Int_set.t -> string*)

let string_of_set f s =
  let f i acc = (f i) :: acc in
  "[" ^ String.concat "," (List.rev (Int_set.fold f s [])) ^ "]"

(*val string_of_list : ('a -> string) -> 'a list -> string*)

let string_of_list f l =
  "[" ^ String.concat ";" (List.map f l) ^ "]"

(*val string_of_array : ('a -> string) -> 'a array -> string*)

let string_of_array f arr =
  "[|" ^ String.concat ";" (List.map f (Array.to_list arr)) ^ "|]"

(*val string_of_augmented_regexp : augmented_regexp -> string*)

let rec string_of_augmented_regexp x =
  let string_of_pos (p:pos) =
    let {null; first; last} = p in
    Printf.sprintf "{null=%b;first=%s;last=%s}" (null) (string_of_set string_of_int first) (string_of_set string_of_int last) in
  match x with
  | Epsilon -> "Epsilon"
  | Character (c, i) -> Printf.sprintf "Character ('%c', %d)" c i
  | Sequence (x, y, p) -> Printf.sprintf "Sequence (%s, %s, %s)" (string_of_augmented_regexp x) (string_of_augmented_regexp y) (string_of_pos p) 
  | Alternative (x, y, p) -> Printf.sprintf "Alternative (%s, %s, %s)" (string_of_augmented_regexp x) (string_of_augmented_regexp y) (string_of_pos p) 
  | Repetition (x, p) -> Printf.sprintf "Repetition (%s, %s)" (string_of_augmented_regexp x) (string_of_pos p) 
  | Accept i -> Printf.sprintf "Accept %d" i

(*val compute_follow : Int_set.t array -> char option array -> augmented_regexp -> unit*)

let compute_follow follow chars (x:augmented_regexp) =
  let rec compute x = 
    match x with
    | Sequence (e1, e2, p) ->
      compute e1; compute e2;
      let first2 = first_pos e2 in
      let f i =
        follow.(i) <- Int_set.union first2 (follow.(i)) in
      Int_set.iter f (last_pos e1)
    | Repetition (e, p) ->
      compute e;
      let f i =
        follow.(i) <- Int_set.union (p.first) (follow.(i)) in
      Int_set.iter f (p.last)
    | Alternative (e1, e2, p) -> compute e1; compute e2
    | Epsilon -> ()
    | Accept i -> chars.(i) <- None
    | Character (c, i) -> chars.(i) <- Some c in
  compute x

(*val regexp_follow : string -> augmented_regexp * Int_set.t array * char option array*)

let regexp_follow s = 
  let re, n = parse_augmented_regexp s in
  let follow = Array.make n (Int_set.empty) in
  let chars = Array.make n None in
  compute_follow follow chars re;
  (re, follow, chars)

(*val string_of_follow_result : augmented_regexp * Int_set.t array * char option array -> string*)

let string_of_follow_result (e, follow, chars) =
  Printf.sprintf "%s,\n%s, %s" 
    (string_of_augmented_regexp e) 
    (string_of_array (string_of_set string_of_int) follow) 
    (string_of_array (function | None -> "None" | Some c -> "Some '"^String.make 1 c^"'") chars)

type state = {
  pos : Int_set.t;
  mutable trans : transitions ;
} and
transitions = (char * state) list

let string_of_transition (c, st) =
  Printf.sprintf "'%c' -> %s" c (string_of_set string_of_int st.pos)
let string_of_transitions l =
  "[" ^ String.concat " or " (List.map string_of_transition l) ^ "]"
let string_of_state s =
  Printf.sprintf "{pos=%s;trans=%s}\n" (string_of_set string_of_int s.pos) (string_of_transitions (s.trans))

(*val partition : char option array -> Int_set.t -> (char option * Int_set.t) list*)

let partition chars s =
  let f acc c =
    match c with
    | Some _ ->
      if List.mem_assoc c acc then acc 
      else
        let f i acc = if chars.(i) <> c then acc else Int_set.add i acc in
        let s' =  Int_set.fold f s (Int_set.empty) in
        (c, s') :: acc
    | None -> if List.mem_assoc c acc then acc else (c, Int_set.empty) :: acc in
  List.rev (Array.fold_left f [] chars)

(*val list_of_set : Int_set.t -> Int_set.elt list*)

let list_of_set s =
  let f e acc = e :: acc in
  List.rev (Int_set.fold f s [])

(*val accessible : state -> Int_set.t array -> char option array -> (char * Int_set.t) list*)

let accessible s follow chars =
  let part = partition chars s.pos in
  let f p rest =
    match p with
    | (Some c, l) -> 
      (c,
       List.fold_left 
         (Int_set.union) 
         (Int_set.empty) 
         (List.map (Array.get follow) (list_of_set l))
      ) :: rest
    | _ -> rest  in
  List.fold_right f part []

(*val find_state : Int_set.t -> state list -> state list -> state*)

let find_state s l m =
  let test e = e.pos = s in
  try
    List.find test l
  with
  | Not_found -> List.find test m

(*val compute_states : state list -> state list -> Int_set.t array -> char option array -> state array*)

let rec compute_states marked unmarked follow chars =
  match unmarked with
  | [] -> Array.of_list marked
  | st :: umsts ->
    let access = accessible st follow chars in
    let marked1 = st :: marked in
    let f (c, s) umsts =
      if Int_set.is_empty s then 
        umsts (*Suppress empty sets*)
      else
        try
          st.trans <- (c, find_state s marked1 umsts) ::st.trans ;
          umsts
        with
        | Not_found -> 
          let state1 = {pos = s; trans = []} in
          st.trans <- (c, state1) :: st.trans;
          state1 :: umsts in
    let unmarked1 = List.fold_right f access umsts in
    compute_states marked1 unmarked1 follow chars

(*val array_indexq : 'a array -> 'a -> int*)

let array_indexq arr e =
  let rec loop i =
    if i = Array.length arr then
      raise (Not_found)
    else if Array.get arr i == e then i
    else loop (i + 1) in
  loop 0

(*val dfa_of : augmented_regexp * Int_set.t array * char option array -> state array*)

let dfa_of (e, follow, chars) =
  let init_state = {pos = first_pos e; trans = []} in
  let dfa = compute_states [] [init_state] follow chars in
  (*Installing initial state at index 0*)
  let idx_start = array_indexq dfa init_state in
  dfa.(idx_start) <- dfa.(0);
  dfa.(0) <- init_state;
  dfa

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

type parsed =
  | Returns of char option* (char list)
  | Analyze_fails

type parser = char list -> parsed

let (token : (char -> char option) -> parser) =
  let f test l =
    match l with
    | (t :: ts) -> 
       begin
         match test t with
         | Some r -> Returns (Some r, ts)
         | None -> Analyze_fails
       end
    | _ -> Analyze_fails in
  f

let end_of_input l = 
  match l with
  | [] -> Returns (None, [])
  | _ -> Analyze_fails

let parser_of_char c = token (fun c' -> if c = c' then Some c else None)

let (or_else : parser -> parser -> parser) =
  fun p1 p2 toks ->
  match p1 toks with
  | Analyze_fails -> p2 toks
  | Returns (c, cs) -> Returns (c, cs)

let (and_also : parser -> parser -> parser) =
  fun p1 p2 toks ->
  match p1 toks with
  | Analyze_fails -> Analyze_fails
  | Returns (c, cs) -> p2 cs

(*val epsilon : char list -> parsed*)

let epsilon l = 
  Printf.printf "epsilon" ;
  Returns (None, l)

(*val parser_or : parser -> parser list -> parser*)

let parser_or p pl = List.fold_right or_else pl p

let interpret_dfa dfa accept =
  let num_states = Array.length dfa in
  let fvect = 
    Array.make (num_states) 
               (fun _ -> failwith "unexpected : no value") in
  for i = 0 to num_states - 1 do
    let trans = dfa.(i).trans in
    let f (c, st) =
      let pc = parser_of_char c in
      let j = array_indexq dfa st in
      and_also pc (fun l -> fvect.(j) l) in
    let parsers = List.map f trans in
    if Int_set.mem accept (dfa.(i).pos) then
      fvect.(i) <- parser_or end_of_input parsers
    else match parsers with
         | [] -> failwith "Impossible"
         | p :: ps -> fvect.(i) <- parser_or p ps
  done;
  fvect.(0)

let compile xpr = 
  let ((e, follow, chars) as ast) = regexp_follow xpr in
  let dfa = dfa_of ast in
  let parser = interpret_dfa dfa (Array.length chars - 1) in
  fun s -> parser (explode s)
