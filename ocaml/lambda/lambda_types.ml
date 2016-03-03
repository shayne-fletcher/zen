module type LAMBDA = sig
type t =
| Free of string
| Bound of int
| Abs of string * t
| Apply of t * t
val abstract : int -> string -> t -> t
val abstract_list : string list * t -> t
val apply_list : t * t list -> t
val subst : int -> t -> t -> t
val inst : t String_dict.t -> t -> t
end

type t =
| Free of string
| Bound of int
| Abs of string * t
| Apply of t * t

(*Convert ocurrences of [b] to bound index [i] in a term*)
let rec abstract i b t =
  match t with
  | Free a -> if a = b then Bound i else Free a
  | Bound j -> Bound j
  | Abs (a, t) -> Abs (a, abstract (i + 1) b t)
  | Apply (t, u) -> Apply (abstract i b t, abstract i b u)

(*Abstraction over several free variables*)
let abstract_list (bs, t) =
  List.fold_right (fun b u -> Abs (b, abstract 0 b u)) bs t

let apply_list (t0, us) =
  List.fold_left (fun t u -> Apply (t, u)) t0 us

(*Shift a term's non-local indicies by [i]*)
let rec shift i d u =
  if i = 0 then u
  else
    match u with
    | Free a -> Free a
    | Bound j -> if j >= d then Bound (j + i) else Bound j
    | Abs (a, t) -> Abs (a, shift i (d + 1) t)
    | Apply (t, u) -> Apply (shift i d t, shift i d u)

(*Substitute [u] for bound variable [i] in a term [t]*)
let rec subst i u t =
  match t with
  | Free a -> Free a
  | Bound j -> 
    if j < i then Bound j (*locally bound*)
    else if j = i then shift i 0 u
    else (*j > i*) Bound (j - 1)  (*non-local to [t]*)
  | Abs (a, t) -> Abs (a, subst (i + 1) u t)
  | Apply (t1, t2) -> Apply (subst i u t1, subst i u t2)

(*Substitution for free variables. This function expects to be given
  proper \lambda terms having no unmatched indices. It therefore
  does not keep track of the nesting depth or call [shift]*)
let rec inst env t = 
  match t with
  | Free a -> 
    begin
      try
        inst env (String_dict.find a env)
      with
      | Not_found -> Free a
    end
  | Bound i -> Bound i
  | Abs (a, t) -> Abs (a, inst env t)
  | Apply (t1, t2) -> Apply (inst env t1, inst env t2)
