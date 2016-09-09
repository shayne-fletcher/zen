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

type variable = string
type constructor = string

type pattern = {
  ppat_desc : pattern_desc;
}

and pattern_desc =
| Ppat_var of variable
| Ppat_construct of constructor * pattern list

(*For example, [x :: xs] is represented by 
  {[
     {ppat_desc=
        Ppat_construct ("::"
           , [{ppat_desc=Ppat_var "x"}; {ppat_desc=Ppat_var "xs"}]
        )}
  ]}
*)

and expression = {
  pexp_desc : expression_desc;
}

and expression_desc =
| Pexp_case of variable * clause list
| Pexp_fatbar of expression * expression
(* (| ...) *)
| Pexp_var of variable

and clause =
| Pexp_clause of constructor * variable list * expression

(*The '...' in the definition of type [expression_desc] stands for
  other constructors used to represent other expressions, such as
  variables, applications and lambda abstractions. We do not need to
  know anything about these other expressions, except that there is a
  substitution function defined for them*)
let rec subst (e : expression) (s : variable) (t : variable) : expression =
  (*Replace occurences of [x] in [l] with [y]
  *)
  (*
  let replace (x : 'a) (y : 'a) (l : 'a list) : 'a list =
    List.fold_right (fun e acc -> (if e = x then y else x) :: acc) l [] in
  *)
  failwith "Not implemented"
(*For example, if E represents the expression (f x y), then [subst E
  "_u1" "x"] represents the expression (f _u1 y)*)

(*The function [arity], given a constructor, returns its arity*)
let arity : constructor -> int = function
  | "[]" -> 0
  | "::" -> 2
  | _ as s -> failwith ("arity : \"" ^ s ^ "\" is not a constructor")

(*[constructors s] produces a list of all of the constructors of the
  type of [s]*)
let constructors : constructor -> constructor list = function
  | ("[]" | "::") -> ["[]"; "::"]
  | _ as s -> failwith ("constructors : \"" ^ s ^ "\" is not a constructor")


(*For example, the case-expression
  ```
    case xs of 
      [] -> E1
      y :: ys -> E2
  ```
  will be represented by

  ```
  Pexp_case ("xs", [Pexp_clause ("[]", [], E1);
               Pexp_clause ("::", ["y"; "ys"], E2)]
*)

(*An equation is a list of patterns paired with an expression*)
type equation = pattern list * expression

(*We need functions to determine if an equation begins with a variable
  of a constructor*)
let is_var : equation -> bool = function
  | {ppat_desc = Ppat_var _} :: ps, _ -> true
  | {ppat_desc = Ppat_construct (c, _)} :: ps, _ -> false
  | _ -> assert false
let is_con (e : equation) : bool = not (is_var e)
(*We also need a function to return that constructor*)
let get_con (e : equation) : constructor =
  match e with
  | {ppat_desc = Ppat_construct (c, ps') } :: ps, _ -> c
  | _ -> assert false

(*Generate variable names*)
let make_var (n : int) : variable = "v" ^ (string_of_int n)

(**The call [partition f xs] computes a list of lists, $xs_\{1\},
   xs_\{2\}, \dots, xs_\{n\}$ such that $xs = \cup_\{i =
   1\}^\{n\}xs_\{i\}$ and such that $f\;x = f\;x'$ for any elements $x$
   and $x'$ in $xs_\{i\}$, $i$ from $1$ to $n$, and such that $f\;x \neq
   f\;x'$ for any elements $x$ in $xs_\{i\}$ and $x'$ in $xs_\{i +
   1\}$*)
let rec partition (f : 'a -> 'b) : 'a list -> 'a list list = function
  | [] -> []
  | [x] -> [[x]]
  | x :: x' :: xs when f x = f x' ->  tack x (partition f (x' :: xs))
  | x :: x' :: xs -> [x] :: partition f (x' :: xs)
and tack 
    (x : 'a) 
    (xss : 'a list list) : 'a list list =
  (x :: List.hd xss) :: (List.tl xss) 

(*e.g.
# partition odd [1; 3; 2; 4; 1] ;;
- : int list list = [[1; 3]; [2; 4]; [1]]
*)

(*A running example. We aim to translate the definition
  {[
    let rec map_pairs f xs ys = 
      match (xs, ys) with
      | [], ys -> []
      | (x :: xs), [] -> []
      | (x :: xs), (y :: ys) -> f x y :: map_pairs f xs ys
   ]}

  to the equivalent case expression,

  ```map_pairs = 
    \u1.\u2.\u3.
    case u2 of
      [] -> NIL
      u4 :: u5 -> case u3 of
                     [] -> []
                     u6 :: u7 -> (u1 u4 u6) :: (map_pairs u1 u5 u7)
    ```
*)

let range 
    (s : int) 
    (e : int) : int list =
  let rec loop acc s e =
    if s >= e then acc
    else loop (s :: acc) (s + 1) e 
  in List.rev (loop [] s e)

(*The function [match_]*)

(*Calls have the form [match_ k us qs def]. 

  [us] is a list of variables, [qs] a list of equations and [def] a
  default expression. The argument [k] is added to help in generating
  new variable names; it should be chosen so that for every $i > k$,
  [make_var i] is a new variable not in use in [us], [qs] or [def].*)

let rec match_ 
    (k : int) 
    (us : variable list)
    (qs : equation list)
    (def : expression) : expression =
  match (k, us, qs, def) with

  (*The empty rule*)

  | k, [], qs, def -> 
    let h (l, e) = 
      match l with 
      | (_ :: _) -> assert false 
      | [] -> e in
    let es = List.map h qs in
    let f x acc = {pexp_desc=Pexp_fatbar (x, acc)} in
    List.fold_right f es def

  (* The mixture rule *)

  | k, (u :: us), qs, def ->
    List.fold_right (match_var_con k (u :: us)) (partition is_var qs) def

and match_var_con 
    (k : int) 
    (us : variable list) 
    (qs : equation list) 
    (def : expression) : expression =
  if is_var (List.hd qs) then 
    match_var k us qs def
  else 
    match_con k us qs def

and match_var
    (k : int) 
    (us : variable list) 
    (qs : equation list) 
    (def : expression) : expression =

  match us with
  | [] -> assert false
  | (u :: us) ->

  (*The variable rule*)

    let h (l, e) = 
      match l with
      | {ppat_desc=Ppat_var v} :: ps -> (ps, subst e u v)
      | _ -> assert false in
    let qs' = List.map h qs in
    match_ k us qs' def

and match_con 
    (k : int) 
    (us : variable list) 
    (qs : equation list) 
    (def : expression) : expression =
  match us with
  | [] -> assert false
  | (u :: us) ->

    (*The constructor rule*)

    let cs = constructors (get_con (List.hd qs)) in
    let f c = match_clause c k (u :: us) (choose c qs) def in
    let clauses = List.map f cs in
    {pexp_desc=Pexp_case (u, clauses)}

and match_clause 
    (c : constructor) 
    (k : int) 
    (us : variable list)
    (qs : equation list)
    (def : expression) : clause =

  let k' : int = arity c in
  let us' : variable list = 
    List.map (fun i -> make_var (i + k)) (range 1 (k' + 1)) in

  let f e =
    match e with
    | {ppat_desc = Ppat_construct (c, ps')} :: ps, e -> (ps' @ ps, e)
    | _ -> assert false in
  let qs' : equation list = List.map f qs in

  Pexp_clause (c, us', match_ (k' + k) (us' @ us) qs' def)

and choose (c : constructor) (qs : equation list) : equation list =
  List.filter (fun e -> (get_con e) = c) qs

(* -- *)

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

let pair 
    (i : int)
    (f : int -> formatter -> 'a -> 'b)
    (ppf:formatter) 
    ((u, v) : 'a * 'a ) : unit =
    line i ppf "(\n";
    f (i + 1) ppf u;
    f (i + 1) ppf v;
    line i ppf ")\n"

let fmt_ident (ppf : formatter) (x : string) : unit =
  fprintf ppf "\"%s\"" x

let ident (i : int) (ppf : formatter) (x : string) : unit =
  line i ppf "%a" fmt_ident x

let rec pattern (i : int) (ppf : formatter) (x : pattern) : unit =
  line i ppf "pattern\n";
  let i = i + 1 in
  match x.ppat_desc with
  | Ppat_var v ->    
    line i ppf "Ppat_var %a\n" fmt_ident v
  | Ppat_construct (c, pl) -> 
    line i ppf "Ppat_construct %a\n" fmt_ident c;
    list (i + 1) pattern ppf pl

and expression (i : int) (ppf : formatter) (x : expression) : unit =
  line i ppf "expression\n";
  let i = i + 1 in
  match x.pexp_desc with
  | Pexp_fatbar (e1, e2) -> 
    line i ppf "Pexp_fatbar\n";
    pair i expression ppf (e1, e2)
  | Pexp_case (v, cl) -> 
    line i ppf "Pexp_case %a\n" fmt_ident v;
    list (i + 1) clause ppf cl
  | Pexp_var v ->
    line i ppf "Pexp_var %a\n" fmt_ident v

and clause (i : int) (ppf : formatter) (c : clause) : unit =
  line i ppf "clause\n";
  let i = i + 1 in
  match c with
  | Pexp_clause (c, vs, e) ->
    line i ppf "%a\n" fmt_ident c;
    list i ident ppf vs;
    expression i ppf e

let string_of_pattern (p : pattern) : string =
  pattern 0 (str_formatter) p;
  flush_str_formatter ()

let string_of_expression (e : expression) : string =
  expression 0 (str_formatter) e;
  flush_str_formatter ()

let () = 
  let p : pattern = 
    {ppat_desc=
       Ppat_construct ("::", 
         [{ppat_desc=Ppat_var "x"}; {ppat_desc=Ppat_var "xs"}])
    } in
  Printf.printf "%s" (string_of_pattern p)

let () =
  let e : expression =
    {pexp_desc=Pexp_case ("xs", 
       [Pexp_clause("[]", [], {pexp_desc=Pexp_var "e"});
        Pexp_clause("::", ["y"; "ys"], {pexp_desc=Pexp_var "e"});
       ])} in
  Printf.printf "%s" (string_of_expression e)
