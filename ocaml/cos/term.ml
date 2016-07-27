(**Code implementing substitutions on terms with variables*)

(**{%html:
   <script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js">
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
   $L = \lbrack y_\{1\}/s_\{1\}, y_\{2\}/s_\{2\},
   \dots,y_\{m\}/s_\{m\}\rbrack$
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
