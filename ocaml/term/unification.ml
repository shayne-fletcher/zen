(*The type of a term*)
type ('a, 'b) term =
| Var of 'b
| Term of 'a * ('a, 'b) term list

(*
  Assume [term_fold] to operate on arguments of type [('a, 'd)
  term]. It "returns" either via [f] or [v], and so [f], [v] and
  [term_fold] must share the same return type, ['c] say.

  Since the intermediate list results from recursive calls to
  [term_fold] then it must have type ['c list]. This means [g] must
  be of type ['c -> 'b -> 'b] for some type ['b] which fixes [x] to
  ['b] whilst completing [f] as ['a * 'b -> 'c].

  Lastly [v] takes arguments of type ['d] meaning it types to ['d ->
  'c].
*)
let rec term_fold 
    (f : ('a * 'b) -> 'c)
    (g : 'c -> 'b -> 'b)
    (x : 'b)
    (v : 'd -> 'c) : ('a, 'd ) term -> 'c =  function
| Term (a, tl) ->
  let l : 'c list = List.map (term_fold f g x v) tl in
  let res : 'b = List.fold_right g l x in
  f (a, res)
| Var b -> v b

(*Compute the size of a term*)
let term_size (t : ('a, 'b) term) : int = 
  term_fold (fun (_, s) -> s + 1) (fun x y -> x + y) 0 (fun _ -> 1) t

(*Check if a variable occurs in a term*)
let rec occurs (x : 'b) : ('a, 'b) term -> bool = function
  | Var y -> x = y
  | Term (_, s) -> List.exists (occurs x) s

(*The type of a substitution*)
type ('a, 'b) substitution = ('b * ('a, 'b) term) list

(*Substitue term [s] for all occurences of variable [x] in term [t]*)
let rec subst (s : ('a, 'b) term) (x : 'b) (t : ('a, 'b) term) : ('a, 'b) term =
  match t with
  | Var y as v -> if x = y then s else v
  | Term (f, u) -> Term (f, List.map (subst s x) u)

(*Apply a substitution, right to left*)
let rec apply (s : ('a, 'b) substitution) (t : ('a, 'b) term) : ('a, 'b) term =
  List.fold_right (fun ((x : 'b), (u : ('a, 'b) term)) -> subst u x) s t

(*Compose substitutions*)
let compose_subst (u : ('a, 'b) substitution) (v : ('a, 'b) substitution) : ('a, 'b) substitution =
  (*Apply the substitutions in [u] to the terms in [v]*)
  let v' : ('a, 'b) substitution = List.map (fun (x, t) -> (x, apply u t)) v in
  (*Variables in [v]*)
  let vs : 'b list = List.map fst v in
  (*Filter out substitutions in [u] that are in variables for which
    there is a substitution in in [v]*)
  let u' : ('a, 'b) substitution = List.filter (fun (x, _) -> not (List.mem x vs)) u in
  v' @ u'
(*We get the image in the composition of a variable modified by [v]
  when we take the image by [u] by its image in [v]. The image in the
  composition of a variable not modified by [v] is its image by [u]*)

(*Substitue term [s] for all occurences of variable [x] in term [t]*)
let rec subst (s : ('a, 'b) term) (x : 'b) : ('a, 'b) term -> ('a, 'b) term = function
  | Var y as v -> if x = y then s else v
  | Term (a, u) -> Term (a, List.map (subst s x) u)

(*Apply a substitution, right to left*)
let apply (s : ('a, 'b) substitution) (t : ('a, 'b) term) : ('a, 'b) term =
  List.fold_right (fun (x, u) -> subst u x) s t

(*Unify one pair*)
let rec unify_one (s : ('a, 'b) term) (t : ('a, 'b) term) : ('a, 'b) substitution =
  match (s, t) with
  | (Var x, Var y) -> if x = y then [] else [(x, t)]
  | (Term (f, sc), Term (g, tc)) ->
    if f = g && List.length sc = List.length tc
    then unify (List.combine sc tc)
    else failwith "unification failure : head symbol conflict"
  | (Var x, (Term (_, _) as t))
  | ((Term (_, _) as t), Var x) -> 
    if occurs x t
    then failwith "unification failure : circularity"
    else [(x, t)]

(*Unify a list of pairs of terms*)
and unify (s : (('a, 'b) term * ('a, 'b) term) list) : ('a, 'b) substitution =
  match s with
  | [] -> []
  | (x, y) :: t ->
    let t2 = unify t in
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    t1 @ t2
