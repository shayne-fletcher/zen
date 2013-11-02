include Term_types

(*
type ('a, 'b) term = 
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
                "file \"<string>\", line %d, character %d\n\
Error : Syntax error \"%s\"" line cnum tok
             )
          )
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

   [{val term_trav :
     f:('a * 'b -> 'c) ->
     g:('c -> 'b -> 'b) -> x:'b -> v:('d -> 'c) -> ('a, 'd) term -> 'c = <fun>
   }]
*)
let rec term_trav ~f ~g ~x ~v = function
  | Term (a, tl) -> 
    let l = List.map (term_trav ~f ~g ~x ~v) tl in
    let res = List.fold_right g l x in
    f (a, res)
  | Var b -> v b
;;

(*[val size : ('a, 'b) term -> int = <fun>]*)
let size (t:('a, 'b) term) : int = 
  (term_trav 
     ~f:(fun (_, s) -> s + 1) ~g:(fun x y -> x + y) ~x:0 ~v:(fun _ -> 1))
    t
;;

(*[val vars : ('a, 'b) term -> 'b list = <fun>]*)
let vars t = term_trav ~f:snd ~g:union ~x:[] ~v:(fun x -> [x]) t
;;

(*[val occurs : 'a -> ('b, 'a) term -> bool = <fun>]*)
let occurs v t = List.mem v (vars t)
;;

(*
  [{val apply_substitution : 
     ('a * ('b, 'a) term) list -> 
            ('b, 'a) term -> ('b, 'a) term =
  <fun>}]
*)
let apply_substitution subst t = 
  term_trav 
    ~f:(fun (f, l) -> Term (f, l)) 
    ~g:(fun x acc -> x::acc) 
    ~x:[] 
    ~v:(fun s -> try List.assoc s subst with _ -> Var s) 
    t

(*
  [{val apply_subst : ('a * ('b, 'a) term) list -> 
                          ('b, 'a) term -> ('b, 'a) term =
  <fun>}]
*)
let rec apply_subst subst = function
  | Term (f, ns) -> Term (f, List.map (apply_subst subst) ns)
  | Var x as v -> try List.assoc x subst with _ -> v
;;

let compose_subst sig1 sig2 =
  (*First, take the list corresponding to sig2 and apply to its
    terms, the substitutions in sig1...*)
  (List.map (fun (v, t) -> (v, apply_substitution sig1 t)) sig2)
  (*... now, remove variables from sig1 that are replaced under
    application of the sig2 substitutions and concatenate the
    results*)
  @(let vs = List.map fst sig2 in 
    List.filter (fun (x, t) -> not (List.mem x vs)) sig1)
;;

exception Term_match_exc ;;

(*[{val extend_subst : ('a * 'b) list -> 
                        ('a * 'b) list -> ('a * 'b) list = <fun>}]*)
let extend_subst s1 s2 =
  let f acc (x, t)= 
    try  
     let u = List.assoc x acc in
      if t = u then acc
      else raise Term_match_exc
    with Not_found -> (x, t)::acc
  in List.fold_left f s1 s2
;;

let term_match (t1, t2) =
  let rec term_match' subst = function
    | (Var v, t) -> extend_subst [v, t] subst
    | (t, Var v) -> raise Term_match_exc
    | (Term (f, l), Term (g, r)) ->
      if f = g then 
        List.fold_left term_match' subst (List.combine l r)
      else raise Term_match_exc
  in term_match' [] (t1, t2)
;;

(*
let tt = term_of_string "a(b(), c)" in print_term tt

let s = ["x", term_of_string "g(z)"] in
print_term (apply_substitution s (term_of_string "f (x, y, x)"))
*)

let sig1 = ["x", term_of_string "g (x, y)"] in
let sig2 = ["y", term_of_string "h (x, z)"; "x", term_of_string "k(x)"] in
let subst=compose_subst sig1 sig2 in
print_string "y -> " ; print_term (List.assoc "y" subst) ; print_newline ();
print_string "x -> " ; print_term (List.assoc "x" subst) ; print_newline ()

(*
let t = term_of_string "h(y(z, k(x, s)))"
and u = term_of_string "h(y(l(m, n, o()), k(t(), u)))" in
let matching = term_match (t, u) in
print_string "x -> " ; print_term (List.assoc "x" matching) ; print_newline () ;
print_string "z -> " ; print_term (List.assoc "z" matching) ; print_newline () ;
print_string "s -> " ; print_term (List.assoc "s" matching) ; print_newline ()
*)
