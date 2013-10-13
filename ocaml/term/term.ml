include Term_types
(*type ('a, 'b) term = 
| Term of 'a * ('a, 'b) term list
| Var of 'b
;;
*)

(*Unordered set operations (on list representations)*)

let add_to_set a l =
  if List.mem a l then l else (a::l)
;;

let union l1 l2 = 
  List.fold_right add_to_set l2 l1
;;

let term_of_string s =
  let parse_buf lexbuf =
    try Parser.main Lexer.token lexbuf
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
                "file \"<string>\", line %d, character %d\n\Error : Syntax error \"%s\"" line cnum tok))
      end
  in parse_buf (Lexing.from_string s)

let rec string_of_term (t:(string, string)term) : string = 
  let string_of_list (f:'a -> string) (l:'a list) : string = 
    "(" ^ String.concat "," (List.map f l) ^ ")"  in
  match t with
  | Term (s, tl) -> Printf.sprintf "%s" (s^(string_of_list string_of_term tl))
  | Var s -> s
;;

let print_term (t:(string, string) term) : unit =
  Printf.printf "%s" (string_of_term t)
;;

(* [term_trav] is a generalization of all functions defined by
   recusion on the structure of an [('a, 'b) term] - a homomorphism.

   The definition of [term_trav] below gives the following signature.

   {[
   val term_trav : ('a * 'b -> 'c) ->
   ('c -> 'b -> 'b) -> 'b -> ('d -> 'c) -> ('a, 'd) term -> 'c
   ]}

   Proof:

   Assume [term_trav] to operate on arguments of type [('a, 'd)
   term]. It "returns" either via [f] or [v], and so [f], [v] and
   [term_trav] must share the same return type, ['c] say.

   Since the intermediate list results from recursive calls to
   [term_trav] then it must have type ['c list]. This means [g] must
   be of type ['c -> 'b -> 'b] for some type ['b] which fixes [x] to
   ['b] whilst completing [f] as ['a * 'b -> 'c].

   Lastly [v] takes arguments of type ['d] meaning it types to ['d ->
   'c].

*)
let rec term_trav f g x v = function
  | Term (a, tl) -> 
    let l = List.map (term_trav f g x v) tl in
    let res = List.fold_right g l x in
    f (a, res)
  | Var b -> v b
;;

let term_size (t:('a, 'b) term) : int = 
  (term_trav (fun (_, s) -> s + 1) (fun x y -> x + y) 0 (fun _ -> 1))
    t
;;

(*[val vars : ('a, 'b) term -> 'b list = <fun>]*)
let term_vars t = term_trav snd union [] (fun x -> [x]) t
;;

(*[val occurs : 'a -> ('b, 'a) term -> bool = <fun>]*)
let term_occurs v t = List.mem v (term_vars t)
;;

let tt = term_of_string "a(b(), c)" in print_term tt
