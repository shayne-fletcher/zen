open Ml_types

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Ml_longident.t
  | Unbound_type_constructor_2 of Ml_path.t
  | Type_arity_mismatch of Ml_longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Ml_longident.t
  | Type_mismatch of (type_expr * type_expr) list
  | Alias_type_mismatch of (type_expr * type_expr) list
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * type_expr
  | Property_outside_type_declaration
  | Multiple_constraints_on_type of Ml_longident.t
  | Not_a_string_constant
  | Repeated_method_label of string
  | Unbound_value of Ml_longident.t
  | Unbound_constructor of Ml_longident.t
  | Unbound_label of Ml_longident.t
  | Unbound_module of Ml_longident.t
  | Unbound_class of Ml_longident.t
  | Unbound_modtype of Ml_longident.t
  | Unbound_cltype of Ml_longident.t
  | Ill_typed_functor_application of Ml_longident.t
  | Illegal_reference_to_recursive_module
  | Access_functor_as_structure of Ml_longident.t
  | Apply_structure_as_functor of Ml_longident.t
  | Cannot_scrape_alias of Ml_longident.t * Ml_path.t

exception Error of Ml_location.t * Ml_env.t * error
exception Error_forward of Ml_location.error

let rec narrow_unbound_lid_error : 'a. _ -> _ -> _ -> _ -> 'a =
  fun env loc lid make_error ->
  raise (Error (loc, env, make_error lid))

let find_component (lookup : ?loc:_ -> _) make_error env loc lid =
  try
    match lid with
    | Ml_longident.Ldot (Ml_longident.Lident "*predef*", s) ->
        lookup ~loc (Ml_longident.Lident s) Ml_env.initial_safe_string
    | _ ->
        lookup ~loc lid env
  with 
  | Not_found ->
    narrow_unbound_lid_error env loc lid make_error
  | Ml_env.Recmodule ->
    raise (Error (loc, env, Illegal_reference_to_recursive_module))

let find_value env loc lid =
  Ml_env.check_value_name (Ml_longident.last lid) loc;
  let (path, decl) as r =
    find_component 
      Ml_env.lookup_value 
      (fun lid -> Unbound_value lid) env loc lid
  in
  r
