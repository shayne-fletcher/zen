(**Substitutions on terms with variables*)

(**{%html:
   <script type="text/x-mathjax-config">
   MathJax.Hub.Config({
   extensions: ["tex2jax.js","TeX/AMSmath.js","TeX/AMSsymbols.js"],
   jax: ["input/TeX", "output/HTML-CSS"],
   tex2jax: {
   inlineMath: [ ['$','$'], ["\\(","\\)"] ],
   displayMath: [ ['$$','$$'], ["\\[","\\]"] ],
   },
   "HTML-CSS": { availableFonts: ["TeX"] }
   });
   </script> 
   <script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js"></script>
   %}
*)

(**{2 Terms}*)

(**The type of terms with variables*)
type ('a, 'b) term = 
| Term of 'a * ('a, 'b) term list (**An ['a] and a list of child terms*)
| Var of 'b (**A variable of type ['b]*)

(**Homomorphisms over terms:
   {ul {- On encountering a term we recursively map [term_trav] over its
   children;}
   {- We fold [g] from the right through the results to produce
   [res] using [x] as the initial value for the accumulator;}
   {- We combine [a], the value in the node under consideration with
   [res] through [f];}
   {- On encountering a variable compute a result with [v].} }
*)
let rec term_trav ~f ~g ~x ~v = function
  | Term (a, tl) -> 
    let l = List.map (term_trav ~f ~g ~x ~v) tl in
    let res = List.fold_right g l x in
    f (a, res)
  | Var b -> v b
(*val term_trav :
  f:('a * 'b -> 'c) ->
  g:('c -> 'b -> 'b) -> x:'b -> v:('d -> 'c) -> ('a, 'd) term -> 'c
*)

(**[term_map f g] applies [f] to interior nodes and [g] to variables*)
let rec term_map (f : 'a -> 'c) (g : 'b -> 'd) : ('a, 'b) term -> ('c, 'd) term = function
  | Term (a, cs) -> Term (f a, List.map (term_map f g) cs)
  | Var n -> Var (g n)

(*[unique l] is the list [l] with duplicates removed*)
(**/**)
let unique l =
  let rec loop acc = function
    | [] -> List.rev acc
    | (h :: tl) -> loop (if List.mem h acc then acc else h :: acc) tl
  in loop [] l
(**/**)

(*[subtract l m] returns the list [l] where all elements structurally
  equal to one in [m] have been removed*)
(**/**)
let subtract (l : 'a list) (m : 'a list) : 'a list =
  let rec loop acc = function
  | [] -> List.rev acc
  | (h :: tl) -> loop (if List.mem h m then acc else h :: acc) tl in
  loop [] l
(**/**)

(*[union l m] appends before [m] all the elements of [l] that are not
  structurally equal to an element of [m]*)
(**/**)
let union (l : 'a list) (m : 'a list)  = (subtract l m) @ m
(**/**)

(**[vars t] is a list of the variables of [t]*)
let vars (t : ('a, 'b) term) : 'b list = 
  term_trav ~f:snd ~g:union ~x:[] ~v:(fun x -> [x]) t

(**[occurs v t] tests for the existence of a variable [v] in [t]*)
let occurs (v : 'b) (t : ('a, 'b) term) : bool = List.mem v (vars t)

(**{2 Substitutions}*)

(**Substitutions are modeled as [('b * ('a, 'b) term) list] values*)

(**{3 Application of substitutions to terms}*)

(**[apply_subst s t] applies the substitution [s] to the term [t]*)
let apply_subst 
    (subst : ('b * ('a, 'b) term) list)
    (t : ('a, 'b) term) : ('a, 'b) term = 
  term_trav 
    ~f:(fun (f, l) -> Term (f, l)) 
    ~g:(fun x acc -> x :: acc) 
    ~x:[] 
    ~v:(fun s -> try List.assoc s subst with _ -> Var s) 
    t

(**For example, suppose $T = \lbrack g(z)/x\rbrack$ is applied to the
   term $f (x, y,z)$, then we'd expect the resulting term be $f (g(z),
   y, z)$.
   {[
   # let t = Term ("f", [Var "x"; Var "y"; Var "z"] (*f (x, y, z)*)) in
      let s = [("x",  Term ("g", [Var "z"])) (*[g(z)/z]*)] (*A substitution*) in
      apply_subst s t ;;
        - : (string, string) term =
    Term ("f", [Term ("g", [Var "z"]); Var "y"; Var "z"])
   ]}
*)

(**{3 Composition of substitutions}*)

(**Composing two substitutions, say $T$ and $L$, produces a new
   substitution $T \circ L$, which when applied to an expression, say $P$,
   results in the same expression as applying first $T$ and then $L$ to
   $P$. That is,
   {%html:
   <p align="center">
   %}
   $T \circ L = L \left( T \left( P \right)\right)$.
   {%html:
   </p>
   %}
   
   For example, if we think about first applying $T = \lbrack y/x,
   f(x)/y, c/z\rbrack$ , then $L = \lbrack d/x, e/y, f/w \rbrack$, we
   can see that any instance of $x$ will first be replaced by $y$ when
   $T$ is applied. This becomes $e$ when $L$ is applied, so the net
   result is that $x$ goes to $e$. The variable $y$ would become
   $f(x)$ which is changed to $f(d)$ by $L$ and $z$ becomes $c$ which
   is unchanged by $L$. Furthermore, the variable $w$ untouched by
   $T$, is replaced by $f$ when $L$ is applied. So the composition, $T
   \circ L = \lbrack e/x, f(d)/y, c/z, f/w\rbrack$.

   Formally, we can define composition of substitutions by saying that
   if we have
   {%html:
   <p align="center">
   %}
   $T = \lbrack t_\{1\}/x_\{1\}, t_\{2\}/x_\{2\},
   \dots,t_\{n\}/x_\{n\}\rbrack$
   {%html:
   </p>
   %}
   and
   {%html:
   <p align="center">
   %}
   $L = \lbrack y_\{1\}/s_\{1\}, y_\{2\}/s_\{2\}, \dots,y_\{m\}/s_\{m\}\rbrack$
   {%html:
   </p>
   %}
   then the composition is found by creating
   {%html:
   <p align="center">
   %}
   $T \circ L = \lbrack L(t_\{1\})/t_\{1\}), L(t_\{2\})/x_\{2\},
   \dots, L(t_\{n\})/x_\{n\}\rbrack \cup \lbrack
   s_\{i\}/y_\{i\}\rbrack$ where $i \in \lbrack 1, m\rbrack$ and
   $y_\{i\}$ does not occur as a variable in $T$.
   {%html:
   </p>
   %}
*)

(**Identifying $T$ with [ts], $L$ with [ls], then [compose_subst ts
   ls] computes the composition $\sigma = T \circ L$*)
let compose_subst 
  (ts : ('b * ('a, 'b) term) list)
  (ls : ('b * ('a, 'b) term) list) 
                    : ('b * ('a, 'b) term) list =
  (*The image of T under L*)
  List.map (fun (v, t) -> v, apply_subst ls t) ts
    @ 
  (*Together with the substitutions in L that affect variables not in
    T*)
    (let vs = List.map fst ts
       in List.filter (fun (x, t) -> not (List.mem x vs)) ls
    )
(*Cosineau & Mauny write
   {[
    let compose_subst 
      (subst1 : ('b * ('a, 'b) term) list) 
      (subst2 : ('b * ('a, 'b) term) list)
                      : ('b * ('a, 'b) term) list =
      List.map (fun (v, t) -> v, apply_subst subst1 t) subst2
        @ (let vs = List.map fst subst2
           in List.filter (fun (x, t) -> not (List.mem x vs)) subst1
        )
    ]}
    that is, they identify [ts] with [subst2], [ls] with [subst1] and
    reverse the order of arguments.
*)

(**[subst_but v s] removes [v] from the domain of definition of [s]*)
let rec subst_but (v : 'b) : ('b * _) list -> ('b * _) list = function
  | [] -> []
  | (x, y) :: t -> if x = v then t else (x, y) :: subst_but v t

(**Iterate the process of removing a variable from the domain of
   definition over a list of variables*)
let rec subst_minus (s : ('b * _) list) : 'b list -> ('b * _) list = function
  | [] -> s
  | (v :: tl) -> subst_minus (subst_but v s) tl

(**{2 Unification}*)

(**Given a set of expression say $E = \lbrace E_\{1\}, E_\{2\}, \dots,
   E_\{n\}\rbrace$, we say that a substitution $\sigma$, {i unifies}
   the set if the result of applying the substitution to each
   expression in the set yields exactly the same result. That is,
   $\sigma(E_\{1\}) = \sigma(E_\{2\})=\cdots=\sigma(E_\{n\})$. We call
   such a substitution a {i unifier} and say that the set $E$ is
   unifiable.

   For example, the set $R(x, y, z), R(w, w, x)$ is unified by the
   substitution $\sigma_\{1\} = \lbrack a/x, a/y, a/z, a/w\rbrack$. It
   is also unified by the substitution $\sigma_\{2\} = \lbrack x/y,
   x/z, x/w\rbrack$.

   A substitution $\sigma$, is a {i most general unifier} (mgu) of a
   set of expressions $E$ if it unifies $E$, and for any unifier
   $\omega$ of $E$, there is a unifier $\lambda$, such that $\omega =
   \sigma\circ\lambda$.

   The idea is that $\sigma$ is less specific then (technically, no
   more specific than) $\omega$, that is, we can substitute for some
   of the variables of $\sigma$ and get $\omega$. Note that there can
   be more than one most general unifier, but such substitutions are
   the same except for variable renaming.

   In the example above, $\sigma_\{2\}$ is the mgu of the set of
   expressions. We can see that $\sigma_\{1\} = \sigma_\{2\}\circ
   \lbrack a/x\rbrack$.

   Sketch of the algorithm for two the terms $t$ and $u$:

   {ul 
   {- If both $t$ and $u$ are reduced to variables $x$ and $y$ and $x
   = y$ then $\lbrack \rbrack$ is a solution. If $x \ne y$ then
   $\lbrack (x, u)\rbrack$ is a solution;}
   {- If $t$ is reduced to a variable $x$ then we have the solution
   $\lbrack(x, u)\rbrack$ {b if} the variable $x$ does not appear in
   the term $u$. If the variable $x$ appears in $u$, then there is no
   solution;}
   {- Likewise, if the term $u$ is
   reduced to a variable $x$, we have the solution $(x, t)$ as long as
   $x$ does not appear in $t$;} 
   {- In other cases, $t$ and $u$ are of the form $f(t_\{1\}, \dots,
   t_\{m\})$ and $g(u_\{1\}, \dots, u_\{n\}):$
   {ul
   {- If the symbols $f$ and $g$ are different or $m \ne n$ then
   unification is not possible. Otherwise, the problem of unifying $t$
   and $u$ is reduced to finding a substitution $\sigma$ such that for
   all $i$, $\sigma(t_\{i\}) = \sigma(u_\{i\})$;} 
   {- So, to handle the problem of unifying two terms $t$ and $u$, we
   must be able to handle the unification of a set of pairs of terms
   simultaneously. To do so, we go about it this way:
   {ul
   {- Given the set of pairs of terms to unify $((t_\{1\},
   u_\{1\}),\dots, (t_\{n\}, u_\{n\}))$, we first unify the sequence
   of pairs $((t_\{2\}, u_\{2\}),\dots, (t_\{n\}, u_\{n\}))$;}
   {- If this unification succeeds and produces a unifier
   $\sigma_\{1\}$, then we next try to unify the pair
   $(\sigma_\{1\}(t_\{1\}), \sigma_\{1\}(u_\{1\}))$. If that succeeds
   and produces a unifier $\sigma_\{2\}$, then the composition
   $\sigma_\{1\}\circ\sigma_\{2\}$ is a unifier for $((t_\{1\},
   u_\{1\}),\dots, (t_\{n\}, u_\{n\}))$.
   }}}}}}
*)

(**[unify_one s t] attempts to find a unifier for the terms [s] and
   [t]*)
let rec unify_one 
    (s : ('a, 'b) term) 
    (t : ('a, 'b) term) : ('b * ('a, 'b) term) list = 
  match (s, t) with
  | Var x, Var y -> if x = y then [] else [(x, Var y)]
  | Term (f, sc), Term (g, tc) ->
    if f = g && List.length sc = List.length tc then
      let (ts : (('a, 'b) term * ('a, 'b) term) list) = List.combine sc tc in
      unify ts
    else failwith "unify_one : head symbol conflict"
  | Var x, (Term (_, _) as t)
  | (Term (_, _) as t), Var x ->
    if occurs x t then failwith "unify_one : circularity"
    else [x, t]
(**[unify s] attempts to find a unifier satisfying all pairs in the
   list [s]*)
and unify (s : (('a, 'b) term * ('a, 'b) term) list) : 
                                 ('b * ('a, 'b) term) list =
  match s with
  | [] -> []
  | (x, y) :: t ->
    let t1 = unify t in
    let t2 = unify_one (apply_subst t1 x) (apply_subst t1 y) in
    compose_subst t1 t2    

(* -- *)

type ml_unop =
| Ml_fst
| Ml_snd

type ml_binop =
| Ml_add | Ml_sub | Ml_mult | Ml_eq | Ml_less

type ml_exp =
| Ml_unit
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
| Ml_let_rec of string * ml_exp * ml_exp

type ml_type =
| Unit_type
| Int_type
| Bool_type
| Pair_type of ml_type * ml_type
| Arrow_type of ml_type * ml_type
| Var_type of string

let var (n : int) : ('a, string) term = 
  Var ("v" ^ (string_of_int n))

let const (c : 'a) : ('a, 'b) term = 
  Term (c, [])

let pair 
    ((u : (string, 'b) term),
     (v : (string, 'b) term )) : (string, 'b) term = 
  Term ("pair", [u; v])

let arrow ((u : (string, 'b) term), (v : (string, 'b) term)) = 
  Term ("arrow", [u; v])

let (new_int, reset_int) = 
  let c = ref (-1) in
  (fun () -> c := !c + 1; !c), (fun () -> c := (-1))

let unop_type : ml_unop -> (string, string) term * ('a, string) term = function
  | Ml_fst -> let a = var (new_int ()) and b = var (new_int ())
              in (pair (a, b), a)
  | Ml_snd -> let a = var (new_int ()) and b = var (new_int ())
              in (pair (a, b), b)

let binop_type :
    ml_binop -> (string, 'a) term * (string, 'b) term * (string, 'c) term
    = function
  | Ml_add -> (const "int", const "int", const "int")
  | Ml_sub -> (const "int", const "int", const "int")
  | Ml_mult -> (const "int", const "int", const "int")
  | Ml_eq -> (const "int", const "int", const "bool")
  | Ml_less -> (const "int", const "int", const "bool")

(*[generate_type_constraints] associates a type variable named $v_{i}$
  with each subexpression of number $i$; for each such sub-expression
  being treated, it also produces the set of associated constraints

  The rules it employs are as follows:
*)

let generate_type_constraints (e : ml_exp) 
    :  (('a, string) term * (string, string) term) list =
  let rec gen
      (n : int)
      (tenv : (string * (string, string) term) list)
      : ml_exp -> (('a, string) term * (string, string) term) list
    = function
    | Ml_unit -> [var n, const "unit"]
    | Ml_int_const _ ->
      let v : (_, string) term = var n in
      let t : (string, string) term = const "int" in
      let res : (('a, string) term * (string, string) term) list = [v, t] in 
      res
    | Ml_bool_const _ -> [var n, const "bool"]
    | Ml_unop (op, e) ->
      let ((t1 : (string, string) term), (t2 : ('a, string) term )) = unop_type op 
      and ne : int = new_int () in
      (var n, t2) :: (var ne, t1) :: (gen ne tenv e)
    | Ml_binop (op, e1, e2) ->
      let ((t1 : (string, 'a) term), (t2 : (string, 'b) term), (t3 : (string, 'c) term)) = binop_type op
      and n1 : int = new_int () and n2 : int = new_int () in
      (var n, t3) :: (var n1, t1) :: (var n2, t2) :: (gen n1 tenv e1 @ gen n2 tenv e2)
    | Ml_var x -> [var n, List.assoc x tenv]
    | Ml_pair (e1, e2) ->
      let n1 = new_int () and n2 = new_int () in
      (var n, pair (var n1, var n2)) :: (gen n1 tenv e1 @ gen n2 tenv e2)
    | Ml_if (e1, e2, e3) ->
      let n1 = new_int () and n2 = new_int () and n3 = new_int () in
      (var n1, const "bool") :: (var n, var n2) :: (var n, var n3)
      :: ((gen n1 tenv e1) @ (gen n2 tenv e2) @ (gen n3 tenv e3))
    | Ml_fun (x, e) ->
      let nx = new_int () and ne = new_int () in
      let tenv : (string * (string, string) term) list = (x, var nx) :: tenv in
      (var n, arrow (var nx, var ne)) :: (gen ne tenv e)
    | Ml_app (e1, e2) ->
      let n1 : int = new_int () and n2 : int = new_int () in
      (var n1, arrow (var n2, var n)) :: (gen n1 tenv e1 @ gen n2 tenv e2)
    | Ml_let (s, e1, e2) ->
      let n1 : int = new_int () and n2 : int = new_int () in
      (var n, var n2) :: ((gen n1 tenv e1) @ (gen n2 ((s, var n1) :: tenv) e2))
(*
    | Ml_let_rec (s, e1, e2) ->
      let n1 : int = new_int () and n2 : int = new_int () in
      (var n, var n2) :: (gen n1 ((s, var n1) :: tenv) e1) @ (gen n2 ((s, var n1) :: tenv) e2) 
*)

  in begin 
    reset_int (); 
    gen (new_int ()) [] e 
  end

let rec ml_type_of_term = function
  | Var s -> Var_type s
  | Term ("unit", []) -> Unit_type
  | Term ("int", []) -> Int_type
  | Term ("bool", []) -> Bool_type
  | Term ("pair", [t1; t2]) -> Pair_type (ml_type_of_term t1, ml_type_of_term t2)
  | Term ("arrow", [t1; t2]) -> Arrow_type (ml_type_of_term t1, ml_type_of_term t2)
  | _ -> failwith "Unable to convert term to formula type"
