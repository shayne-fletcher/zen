let rec string_of_regexp re =
  match re with
  | Syntax.Epsilon -> "Epsilon"
  | Syntax.Character c -> Printf.sprintf "Character '%c'" (Char.chr c)
  | Syntax.Sequence (p, q) -> Printf.sprintf "Sequence (%s, %s)" (string_of_regexp p) (string_of_regexp q)
  | Syntax.Alternative (p, q) -> Printf.sprintf "Alternative (%s, %s)" (string_of_regexp p) (string_of_regexp q)
  | Syntax.Repetition r -> Printf.sprintf "Repetition (%s)" (string_of_regexp r)

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

let null_pos x =
  match x with
  | Epsilon -> true
  | Character (_, i) -> false
  | Sequence (_, _, p) -> p.null
  | Alternative (_, _, p) -> p.null
  | Repetition (_, p) -> p.null
  | Accept _ -> false

let first_pos x =
  match x with
  | Epsilon -> Int_set.empty
  | Character (_, i) -> Int_set.add i (Int_set.empty)
  | Alternative (_, _, p) -> p.first
  | Repetition (_, p) -> p.first
  | Sequence (_, _, p) -> p.first
  | Accept i -> Int_set.add i (Int_set.empty)

let last_pos x =
  match x with
  | Epsilon -> Int_set.empty
  | Character (_, i) -> Int_set.add i (Int_set.empty)
  | Alternative (_, _, p) -> p.last
  | Repetition (_, p) -> p.last
  | Sequence (_, _, p) -> p.last
  | Accept i -> Int_set.add i (Int_set.empty)

let epsilon () = 
  Epsilon

and character c = 
  Character (c, generate_label ())

and repetition e = 
  Repetition (e, {null=true;first=first_pos e; last=last_pos e})

and alternative e1 e2 = 
  Alternative (e1, e2, 
               {null=null_pos e1 || null_pos e2;
                first=Int_set.union (first_pos e1)(first_pos e2); 
                last=Int_set.union (last_pos e1) (last_pos e2)})

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

let accept (e:augmented_regexp) = 
  sequence e (Accept (generate_label ()))

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

let parse_augmented_regexp s =
  let () = reset_label () in
  let ast = regexp_of_string s in
  let re1 = augmented_regexp ast in
  let re2 = accept  re1 in
  let count = generate_label () in
  (re2, count)

let string_of_set f s =
  let f i acc = (f i) :: acc in
  "[" ^ String.concat "," (List.rev (Int_set.fold f s [])) ^ "]"

let string_of_list f l =
  "[" ^ String.concat ";" (List.map f l) ^ "]"

let string_of_array f arr =
  "[|" ^ String.concat ";" (List.map f (Array.to_list arr)) ^ "|]"

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

let regexp_follow s = 
  let re, n = parse_augmented_regexp s in
  let follow = Array.make n (Int_set.empty) in
  let chars = Array.make n None in
  compute_follow follow chars re;
  (re, follow, chars)

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

let partition chars s =
  let f acc c =
    match c with
    | Some _ ->
      let f i acc =
        if chars.(i) <> c then acc else Int_set.add i acc in
      let s' =  Int_set.fold f s (Int_set.empty) in
      (c, s') :: acc
    | None -> (c, Int_set.empty) :: acc in
  Array.fold_left f [] chars
  
