(*ML for the working programmer - Paulson
  Writing interpreters for the the \lambda-calculus.
*)

(*[type t] comprises free variables (as strings), bound variables
  (as indices), abstractions and applications
*)
type t =
| Free of string
| Bound of int
| Abs of string * t
| Apply of t * t
  (*Each abstraction node stores the bound variable name for use in
    printing
  *)

val abstract : int -> string -> t -> t
  (*[abstract i b t] converts each occurence of free varaiable [b]
    in [t] to the index [i] (or a greater index within nested
    abstractions)
  *)

val abstract_list : string list * t -> t
  (*[abstract_list ([a; b; c; ...]], t) creates the abstraction [\ a b
    c ... -> t]
  *)

val apply_list : t * t list -> t
  (*Creates the application t a b c ...*)

val subst : int -> t -> t -> t
  (*[subst i t u] substitutes [u] for the bound variable [i] in
    [t]. Usually, [i] = 0 and [t] is the body of an abstraction in the
    beta-conversion (\x.t)u. The case [i] > 0 occurs during recursive
    calls over abstractions in [t]. All indices exceeding i are
    decreased by one to compensate for the removal of that index
  *)

val inst : t String_dict.t -> t -> t
(*[inst env t] copies [t] replacing all occurences of variables
  defined in [env] by thier definitions. The dictionary [env]
  represents an environment and, [inst] expands all the definitions in
  the term. This process is called instantiation. Definitions may
  refer to other definitions; instantiation continues until defined
  variables no longer occur in the result
*)
