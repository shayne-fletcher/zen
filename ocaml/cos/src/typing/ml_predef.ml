(* Predefined type constructors (with special typing rules in
   typecore) *)

open Ml_path
open Ml_types
open Ml_btype

let builtin_idents = ref []

let wrap create s =
  let id = create s in
  builtin_idents := (s, id) :: !builtin_idents;
  id

let ident_create = wrap Ml_ident.create

let ident_int = ident_create "int"
and ident_bool = ident_create "bool"
and ident_unit = ident_create "unit"
and ident_list = ident_create "list"

let path_int = Pident ident_int
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_list = Pident ident_list

let type_int = newgenty (Tconstr(path_int, []))
and type_bool = newgenty (Tconstr(path_bool, [] ))
and type_unit = newgenty (Tconstr(path_unit, [] ))
and type_list t = newgenty (Tconstr(path_list, [t] ))

let decl_abstr =
  {type_params = [];
   type_arity = 0;
   type_kind = Type_abstract;
   type_loc = Ml_location.none;
   type_manifest = None;
   type_newtype_level = None;
  }

let cstr id args =
  {
    cd_id = id;
    cd_args = Cstr_tuple args;
    cd_res = None;
    cd_loc = Ml_location.none;
  }

let decl_abstr =
  {type_params = [];
   type_arity = 0;
   type_kind = Type_abstract;
   type_loc = Ml_location.none;
   type_manifest = None;
   type_newtype_level = None;
  }

let ident_false = ident_create "false"
and ident_true = ident_create "true"
and ident_unit = ident_create "()"
and ident_nil = ident_create "[]"
and ident_cons = ident_create "::"

let common_initial_env add_type empty_env =
  let decl_bool =
    {decl_abstr with
      type_kind = Type_variant ([cstr ident_false []; cstr ident_true []]);
     }
  and decl_unit =
    {decl_abstr with
      type_kind = Type_variant ([cstr ident_unit []]);
     }
  and decl_list =
    let tvar = Ml_btype.newgenvar () in
    {decl_abstr with
     type_params = [tvar];
     type_arity = 1;
     type_kind =
        Type_variant(
          [cstr ident_nil []; 
           cstr ident_cons [tvar; type_list tvar]]);
    }
  in
  add_type ident_list decl_list (
    add_type ident_unit decl_unit (
      add_type ident_bool decl_bool (
        add_type ident_int decl_abstr (
    empty_env)
      )
    )
  )

let build_initial_env add_type empty_env =
  common_initial_env add_type empty_env

(* Start non-predef identifiers at 1000.  This way, more predefs can
   be defined in this file (above!) without breaking .cmi
   compatibility. *)

let _ = Ml_ident.set_current_time 999
let builtin_idents = List.rev !builtin_idents
