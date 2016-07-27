open Term

(*Functions on lists*)

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
    apply_subst renaming t

(*Utilities*)

(*Apply a substitution to a type environment*)
(*
val subst_env :
  (int * ('a, int) term) list ->
  ('b * ('a, int) term scheme) list -> ('b * ('a, int) term scheme) list =
*)
let subst_env subst env =
 List.map 
   (fun (k, For_all (gvars, t)) ->
     (k, For_all (gvars, apply_subst (subst_minus subst gvars) t))) env

(*In order to translate terms into types, we first need to change the
  variables of integer types into strings of characters.*)
let make_string_vars (t : ('a, int) term) : ('a, string) term =
  let var_of_int n = 
    "v" ^ (string_of_int n) in
  term_map (fun x -> x) var_of_int t
