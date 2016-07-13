(*Functions on lists*)

(*[unique l] is the list [l] with duplicates removed*)
let unique l =
  let rec loop acc = function
    | [] -> List.rev acc
    | (h :: tl) -> loop (if List.mem h acc then acc else h :: acc) tl
  in loop [] l

(*[subtract l m] returns the list l where all elements structurally
  equal to one in m have been removed*)
let subtract (l : 'a list) (m : 'a list) : 'a list =
  let rec loop acc = function
  | [] -> List.rev acc
  | (h :: tl) -> loop (if List.mem h m then acc else h :: acc) tl in
  loop [] l

(*[union l m] appends before [m] all the elements of [l] that are not
  structurally equal to an element of [m]*)
let union (l : 'a list) (m : 'a list)  = (subtract l m) @ m

(*[flat_map f l] is (f l1) @ (f l2) @ ... @ (f l n)*)
let rec flat_map f = function
  | [] -> []
  | (h :: tl) -> f h @ flat_map f tl

(*Symbol generators*)

let gen_sym : unit -> string =
  let count = ref (-1) in
  fun () -> count := !count + 1; "ident" ^ (string_of_int !count)

let ((new_int : unit -> int), (reset_new_int : unit -> unit)) = 
  let c = ref (-1) in
  (fun () -> c := !c + 1; !c),
  (fun () -> c := -1)

(*The abstract syntax of expressions*)

type ml_unop = Ml_fst | Ml_snd
type ml_binop = Ml_add | Ml_sub | Ml_mult | Ml_eq | Ml_less

type ml_exp =
| Ml_int_const of int
| Ml_bool_const of bool
| Ml_pair of ml_exp * ml_exp
| Ml_unop of ml_unop * ml_exp
| Ml_binop of ml_binop * ml_exp * ml_exp
| Ml_var of string
| Ml_if of ml_exp * ml_exp * ml_exp
| Ml_fun of string * ml_exp
| Ml_app of ml_exp * ml_exp
| Ml_let of string * ml_exp * ml_exp
| Ml_letrec of string * ml_exp * ml_exp

(*[('a, 'b) term] will underly resentation of types*)

type ('a, 'b) term = 
| Term of 'a * ('a, 'b) term list
| Var of 'b

let rec term_trav ~f ~g ~x ~v = function
  | Term (a, tl) -> 
    let l = List.map (term_trav ~f ~g ~x ~v) tl in
    let res = List.fold_right g l x in
    f (a, res)
  | Var b -> v b

let rec term_map (f : 'a -> 'c) (g : 'b -> 'd) : ('a, 'b) term -> ('c, 'd) term = function
  | Term (a, cs) -> Term (f a, List.map (term_map f g) cs)
  | Var n -> Var (g n)

let vars (t : ('a, 'b) term) : 'b list = 
  term_trav ~f:snd ~g:union ~x:[] ~v:(fun x -> [x]) t

let occurs (v : 'b) (t : ('a, 'b) term) : bool = List.mem v (vars t)

let apply_substitution subst t = 
  term_trav 
    ~f:(fun (f, l) -> Term (f, l)) 
    ~g:(fun x acc -> x::acc) 
    ~x:[] 
    ~v:(fun s -> try List.assoc s subst with _ -> Var s) 
    t

(*Factory functions for terms*)

let var (n : int) : ('a, string) term = Var ("v" ^ (string_of_int n))

let const (c : 'a) : ('a, 'b) term = Term (c, [])

let pair : (string, 'b) term * (string , 'b) term -> (string, 'b) term = 
  fun (u, v) -> Term ("pair", [u; v])

let arrow : (string, 'b) term * (string, 'b) term -> (string, 'b) term =
  fun (u, v) -> Term ("arrow", [u; v])

(*The types of the language are coded by terms in which the variables
  are integers*)

(*The types of basic operators are provided by these functions. They
  furnish a pair or a triple of the types of arguments and the type of
  the result*)

(* val unop_type : ml_unop -> (string, int) term * ('a, int) term*)
let unop_type = function
  | Ml_fst -> let a = Var (new_int ()) and b = Var (new_int ())
              in (pair (a, b), a)
  | Ml_snd -> let a = Var (new_int ()) and b = Var (new_int ())
              in (pair (a, b), b)

(*val binop_type : ml_binop -> (string, 'a) term * (string, 'b) term * (string, 'c) term*)
let binop_type = function
  | Ml_add -> (const "int", const "int", const "int")
  | Ml_sub -> (const "int", const "int", const "int")
  | Ml_mult -> (const "int", const "int", const "int")
  | Ml_eq -> (const "int", const "int", const "bool")
  | Ml_less -> (const "int", const "int", const "bool")

(*We have a new idea available : type schemes. We represent it as
  follows*)
type 'a scheme = For_all of int list * 'a
(*e.g. The type schema A'a. 'a -> 'a is represented as

# For_all ([1], arrow (Var 1, Var 1)) ;;
- : (string, int) term scheme = For_all ([1], Term ("arrow", [Var 1; Var 1]))
*)

(*The following function computes the non-quantified variables that
  appear in the type environment passed to it as as argument*)
let vars_oftyenv (env : ('a * ('b, int) term scheme) list) : int list =
  let f (_, For_all (gvars, t)) = subtract (vars t) gvars in
  flat_map f env

(*The generalization function*)
let generalize (env : ('a * ('b, int) term scheme) list) (t : ('c, int) term ) : ('c, int) term scheme =
  let gvars =
  unique (subtract (vars t) (vars_oftyenv env)) in
  For_all (gvars, t)

(*Given a type scheme, the instantiation function returns a "minimal"
  instance of that scheme. That is, the function simply renames
  the generic variables of the scheme. It is up to unification to
  make the new type variables more precise once they are introduced
  this way.*)
let instance : ('a, int) term scheme -> ('a, int) term = function
  | For_all (gvars, t) ->
    let renaming = List.map (fun n -> (n, Var (new_int ()))) gvars in
    apply_substitution renaming t

(*Utilities*)

(*Now we have type schemes available in type environments (and thus we
  have quantified variables), we must be careful not to substitute them
  when a substitution is applied. The following function removes a
  variable from the definition domain of a substitution.*)
let rec subst_but (v : 'a) : ('a * 'b) list -> ('a * 'b) list = function
  | [] -> []
  | (v1, t1) :: subst ->
    if v1 = v then subst_but v subst
    else (v1, t1) :: (subst_but v subst)

(*If we iterate this process over a list of variables, we remove a set
  of variables from the definition domain of a substitution like this*)
let rec subst_minus (subst : ('a * 'b) list) (vars : 'a list) : ('a * 'b) list =
  match vars with 
  | [] -> subst
  | v :: vs -> subst_minus (subst_but v subst) vs

(*Now we can apply a substitution to a type environment*)
(*
val subst_env :
  (int * ('a, int) term) list ->
  ('b * ('a, int) term scheme) list -> ('b * ('a, int) term scheme) list =
*)
let subst_env subst env =
 List.map 
   (fun (k, For_all (gvars, t)) ->
     (k, For_all (gvars, apply_substitution (subst_minus subst gvars) t))) env

(*In order to translate terms into types, we first need to change the
  variables of integer types into strings of characters.*)
let make_string_vars (t : ('a, int) term) : ('a, string) term =
  let var_of_int n = 
    "v" ^ (string_of_int n) in
  term_map (fun x -> x) var_of_int t
