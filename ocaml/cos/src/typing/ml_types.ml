open Ml_asttypes

type type_expr = {
  mutable desc : type_desc;
  mutable level : int;
  id : int
}

and type_desc =
| Tvar of string option
(** [Tvar (Some "a")] => ['a] or ['_a]
    [Tvar (None)] => [_]*)
| Tarrow of type_expr * type_expr
(**[Tarrow (e1, e2)] => [e1 -> e2]*)
| Ttuple of type_expr list
(** [Ttuple [t1; ...; tn] => (t1 * ... * tn)]*)
| Tconstr of string * type_expr list
(**[Tconstr (t, [t1; ... tn])] => [(t1, ..., tn) t]*)
| Tlink of type_expr 
(**Indirection used by the unification engine*)
| Tsubst of type_expr (*for copying*)
(**[Tsubst] is used temporarily to store information in low-level
   functions manipulating representation of types such as instantiation
   or copy. This constructor should not appear outside of these cases*)
| Tunivar of string option
(**Occurence of a type varaible introduced by a for-all quantifier /
   [Tpoly]*)
| Tpoly of type_expr * type_expr list
(**[Tpoly (ty, tl)] => ['a1... 'an. ty], where 'a1 ... 'an are names
   given to types in tyl and occurences of those types in ty*)

module Type_ops = struct
  type t = type_expr

  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

module Ordered_string = struct
  type t = string
  let compare (x : t) y = compare x y
end
module Vars : Map.S with type key = string = Map.Make (Ordered_string)

type value_description = {
  val_type : type_expr;
  val_kind : value_kind;
  val_loc : Ml_location.t;
}

and value_kind =
| Val_reg (*Regular value*)
(* | Val_prim of Primitive.description   (\* Primitive *\) *)
| Val_unbound (*Unbound variable*)

type type_declaration = {
  type_params : type_expr list;
  type_arity : int;
  type_kind : type_kind;
  type_manifest : type_expr option;
  type_new_type_level : (int * int) option;
  (*definition level * expansion level*)
  type_loc: Ml_location.t;
}

and type_kind = 
| Type_variant of constructor_declaration list

and constructor_declaration = {
  cd_id : Ml_ident.t;
  cd_args : constructor_arguments;
  cd_res : type_expr option;
  cd_loc : Ml_location.t;
}

and constructor_arguments =
| Cstr_tuple of type_expr list

type constructor_description = {
  cstr_name : string; (*Constructor name*)
  cstr_res : type_expr; (*Type of the result*)
  cstr_existentials : type_expr list; (*List of existentials*)
  cstr_args : type_expr list; (*Type of the arguments*)
  (* cstr_arity : int; (\*Number of arguments*\) *)
  (* cstr_tag: constructor_tag; (\* Tag for heap blocks *\) *)
  cstr_consts: int; (* Number of constant constructors *)
  cstr_nonconsts: int; (* Number of non-const constructors *)
  cstr_normal: int; (* Number of non generalized constrs *)
  (* cstr_generalized: bool;  (\* Constrained return type? *\) *)
  (* cstr_private: private_flag;  (\* Read-only constructor? *\) *)
  cstr_loc: Ml_location.t;
  cstr_inlined: type_declaration option;
}
