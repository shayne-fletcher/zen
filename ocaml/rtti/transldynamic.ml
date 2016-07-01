(***************************************************************************)
(*  Copyright (C) 2000-2016 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(* $Id: transldynamic.ml 87666 2016-01-14 16:48:04Z nojebar $ *)

open Misc
open Asttypes
open Types
open Path
open Typedtree
open Lambda
open Mlfi_types

let transl_exp_forward = ref (fun _ -> assert false)
let transl_exp e = !transl_exp_forward e

let lapply ap_func ap_args =
  Lapply
    {
      ap_func;
      ap_args;
      ap_loc = Location.none;
      ap_should_be_tailcall = false;
      ap_inlined = Default_inline;
    }


(* Avoid duplicated warnings *)
let warns = Hashtbl.create 8
let warning loc w =
  let k = (loc, w) in
  if not (Hashtbl.mem warns k) then begin
    Location.prerr_warning loc w;
    Hashtbl.add warns k ();
  end

type error =
  | Illegal_dyn_use
  | Illegal_dyn_type of string * type_expr
  | Type_unification of string

exception Error of Location.t * error
let error loc err = raise (Error (loc, err))

exception Notsimpletype of string
exception Type_unification_error of string
let errstr s = raise(Notsimpletype s)

let path_name loc env path =
  let s = Typecore.full_name_typ env path in
  if String.contains s '*' then warning loc (Warnings.Not_a_global_type s);
  s

let val_name n = n

type use_ttypes_env =
  | U_abstract of Path.t
  | U_id of int

let eq_use_ttype_env u1 u2 =
  match u1, u2 with
  | U_id i1, U_id i2 -> i1 = i2
  | U_abstract p1, U_abstract p2 -> Path.same p1 p2
  | _ -> false

let use_ttypes_env = ref []

let stype_of_type arg =

  let memotbl = Hashtbl.create 16 in
  let env = arg.exp_env in
  let nb_used_types = ref 0 in
  let used_types = ref [] in

  let existing_type lam =
    let k =
      match Lambda.make_key lam with
      | None -> assert false
      | Some k -> k
    in
    let i =
      try
       let (i, _, _) = List.find (fun (_, _, k2) -> k = k2) !used_types in
       i
      with Not_found ->
        let i = !nb_used_types in
        incr nb_used_types;
        used_types := (i, lam, k) :: !used_types;
        i
    in
    DT_var i
  in

  let find_use_type k =
    let (_, id) =
      List.find (fun (k2, _) -> eq_use_ttype_env k k2) !use_ttypes_env
    in
    existing_type (Lvar id)
  in

  let build_dt_prop props t =
    if List.mem ("no_dynamic_type", "") props then
      error arg.exp_loc (Illegal_dyn_type("no_dynamic_type property", arg.exp_type));
    match props with
    | [] -> t
    | _ -> DT_prop(props, t)
  in
  let rec dyn rec_types t =
    let t = Ctype.repr t in
    let (depth, rtypes) = rec_types in
    if depth > 100 then
      errstr "maximum depth exceeded, probably because of non-guarded recursion";
    let rec_types = (depth + 1, rtypes) in
    match t.desc with
    | Tprop (props, t) -> build_dt_prop props (dyn rec_types t)
    | _ -> dyn_no_prop rec_types t t.desc

  and dyn_no_prop rec_types t = function
    | Tprop _ -> assert false
    | Tvar _ ->
        begin
          try find_use_type (U_id t.id)
          with Not_found -> errstr "type variable"
        end
    | Tpoly (t, []) -> dyn rec_types t
    | Tpoly _ -> errstr "poly"
    | Tunivar _ -> errstr "univar"
    | Tarrow (label, t1, t2, _) ->
        (* TODO: should we add a '?' prefix for Optional ? *)
        DT_arrow (Btype.label_name label, dyn rec_types t1, dyn rec_types t2)
    | Ttuple tys -> DT_tuple (List.map (dyn rec_types) tys)
    | Tvariant _ -> errstr "poly variant"
    | Tobject (ty, _) ->
        let (fields, rest) = Ctype.flatten_fields ty in
        begin match rest.desc with
        | Tnil -> ()
        | Tvar _ -> errstr "open object type"
        | _ -> assert false
        end;
        let fields =
          List.fold_right
            (fun (n, k, t) l ->
              let is_mono =
                match (Ctype.repr t).desc with
                | Tpoly (_, []) -> true
                | Tpoly (_, _) -> false
                | _ -> assert false
              in
               match Btype.field_kind_repr k with
               | Fpresent when is_mono -> (n, t) :: l
               | _ -> l)
            fields [] in
        let fields =
          List.sort (fun (n, _) (n', _) -> compare n n') fields in
        DT_object (List.map (fun (s, t) -> (s, dyn rec_types t)) fields)
    | Tsubst _ -> assert false
    | Tpackage(path, l, types) ->
        let s = Path.name path in
        let s =
          match l with
          | [] -> s
          | _ -> Printf.sprintf "%s with types %s" s (String.concat " " (List.map (fun lid -> String.concat "." (Longident.flatten lid)) l))
        in
        DT_abstract(s, List.map (dyn rec_types) types)
    | Tfield(_, _, _, _) | Tnil | Tlink _ -> assert false
    | Tconstr(path, [], _) when Path.same path Predef.path_exn -> DT_abstract ("exn", [])
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_list -> DT_list(dyn rec_types ty_arg)
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_option -> DT_option(dyn rec_types ty_arg)
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array -> DT_array(dyn rec_types ty_arg)
    | Tconstr(path, [], _) when Path.same path Predef.path_int -> DT_int
    | Tconstr(path, [], _) when Path.same path Predef.path_string -> DT_string
    | Tconstr(path, [], _) when Path.same path Predef.path_float -> DT_float
    | Tconstr(path, [], _) when Path.same path Predef.path_char -> DT_abstract ("char", [])
    | Tconstr(path, tys, _) ->
        let tys = List.map Ctype.repr tys in
        let ttys = List.map (dyn rec_types) tys in
        match Path.name path with
        | "Pervasives.date" -> DT_date
        | "Mlfi_acontract.contract"
        | "Mlfi_contract.contract"  -> DT_abstract ("Mlfi_contract.contract", ttys)
        | "Mlfi_contract.observable"
        | "Mlfi_acontract.obs" -> DT_abstract ("Mlfi_contract.observable", ttys)
        | s ->
            let decl =
              try Env.find_type path env
              with Not_found ->
                errstr ("cannot find definition for " ^ s)
            in
            let typexp ty =
              Ctype.remove_props := false;
              try_finally
                (fun () -> Ctype.apply env decl.type_params ty tys)
                (fun () -> Ctype.remove_props := true)
            in

            let abstract_dynamic =
              List.exists (fun (a, _) -> a.txt = "mlfi.abstract_dynamic") decl.type_attributes
            in
            let type_name () =
              match decl with
              | {type_manifest = Some body} when abstract_dynamic ->
                  (* This is used e.g. for type Ib_stdlib.variant, defined as Mlfi_isdatypes.variant, with constructors
                     exported. *)
                  begin match (typexp body).desc with
                  | Tconstr(path, _, _) -> Path.name path
                  | _ ->
                      errstr ("dynamic-abstract type does not expand to path name: " ^ s)
                  end
              | _ ->
                  path_name arg.exp_loc env path
            in

            let try_st_rec set f =
              let args_key = List.map (fun x -> x.id) tys in
              let key = (path, args_key) in
              try Hashtbl.find memotbl key
              with Not_found ->
                let (depth, rtypes) = rec_types in
                if List.length (List.filter ((=) path) rtypes) >= 10 then errstr "non-regular recursion";
                let node = Internal.create_node (type_name ()) ttys in
                let t = DT_node node in
                let props = Ast_helper.get_str_props decl.type_attributes in
                let t = build_dt_prop props t in
                Hashtbl.replace memotbl key t;
                set node (f (dyn (depth, path :: rtypes)));
                t
            in
            match decl with
            | {type_kind = Type_abstract; type_manifest = None} ->
                begin
                  try find_use_type (U_abstract path)
                  with Not_found ->
                      begin
                        try
                          let vpath, vd =
                            match path with
                            | Pdot (m, name, _) ->
                                let s = val_name name in
                                let vd, pos = Env.find_value_in m s env in
                                Pdot (m, s, pos), vd
                            | Pident id ->
                                Env.lookup_value (Longident.Lident (val_name (Ident.name id))) env
                            | Papply _ ->
                                raise Not_found
                          in
                          let ttype t =
                            let p, _ = Env.lookup_type (Longident.parse "Mlfi_types.ttype") env in
                            Ctype.newty (Tconstr (p, [t], ref Mnil))
                          in
                          let et =
                            List.fold_right
                              (fun arg res ->
                                 Ctype.newty (Tarrow (Nolabel, ttype arg, res, Cok))
                              )
                              tys (ttype t)
                          in
                          let ok =
                            Ctype.moregeneral env false et vd.val_type
                          in

                          if ok then begin
(*
                            Format.eprintf "Witness found for abstract type %s: %a@." type_name Location.print arg.exp_loc;
*)
                            if tys <> [] then raise Not_found; (* only non-parametrized type for now *)
                            begin try existing_type (Lambda.transl_path env vpath)
                            with Failure _ -> raise Not_found (* self-recursion *)
                            end
                          end else begin
                            warning
                              (if vd.val_loc.Location.loc_ghost then arg.exp_loc else vd.val_loc)
                              (Warnings.Bad_witness_for_abstract_type (Typecore.full_name_typ env path));
                            raise Not_found;
                          end
                        with Not_found ->
(*                        if (try ignore (String.index type_name '#'); false with Not_found -> true) then *)
                          (* TODO: warning *)
                          DT_abstract (type_name (), ttys)
(*                        else errstr "GADT existential variable" *) (* see #3480 *)
                      end
                end
            | {type_kind = Type_abstract; type_manifest = Some body} when abstract_dynamic ->
                begin match (typexp body).desc with
                | Tconstr(path, tys, _) ->
                    let tys = List.map Ctype.repr tys in
                    let ttys = List.map (dyn rec_types) tys in
                    DT_abstract (Path.name path, ttys)
                | _ -> errstr ("dynamic-abstract type does not expand to path name: " ^ s)
                end
            | {type_kind = Type_abstract; type_manifest = Some body} ->
                assert (not abstract_dynamic);
                dyn rec_types (typexp body)
            | {type_kind = Type_variant constrs} ->
                try_st_rec Internal.set_node_variant begin fun dyn ->
                  let name = Path.last path in
                  let nconst_tag = ref 0 in
                  List.map
                    (fun {Types.cd_id = c; cd_args; cd_res = rt; cd_attributes} ->
                       let c = Ident.name c in
                       Env.mark_constructor_used Env.Positive env name decl c;
                       if rt <> None then errstr "GADT not supported for dynamic types";
                       let ts =
                          match cd_args with
                          | Cstr_tuple [] -> C_tuple []
                          | Cstr_tuple ts ->
                            incr nconst_tag;
                            C_tuple (List.map dyn (List.map typexp ts))
                          | Cstr_record fields ->
                            let fields =
                              List.map
                                (fun {Types.ld_id=s; ld_type=t; ld_attributes} ->
                                   (Ident.name s, Ast_helper.get_str_props ld_attributes, dyn (typexp t))
                                ) fields
                            in
                            let node = Internal.create_node (Printf.sprintf "%s.%s" name c) [] in
                            Internal.set_node_record node (fields, Record_inline !nconst_tag);
                            incr nconst_tag;
                            C_inline (DT_node node)
                       in
                       (c, Ast_helper.get_str_props cd_attributes, ts)

                    ) constrs
                  end
            | {type_kind = Type_record (fields, repr)} ->
                try_st_rec Internal.set_node_record begin fun dyn ->
                  List.map
                    (fun {Types.ld_id=s; ld_type=t; ld_attributes} ->
                       (Ident.name s, Ast_helper.get_str_props ld_attributes, dyn (typexp t))
                    ) fields,
                  match repr with
                  | Types.Record_regular -> Record_regular
                  | Types.Record_float -> Record_float
                  | _ -> assert false
                end
            | {type_kind = Type_open} ->
                errstr "Extensible open types not supported for dynamic types"
  in
  try
    let r = dyn (0, []) arg.exp_type in
    r, List.map (fun (_, lam, _) -> lam) (List.rev !used_types)
  with
  | Notsimpletype i -> error arg.exp_loc (Illegal_dyn_type(i, arg.exp_type))
  | Type_unification_error s -> error arg.exp_loc (Type_unification s)

let transl_longident lid =
  let p, _ =
    try Env.lookup_value (Longident.parse lid) (Env.initial_with_auto ())
    with Not_found ->
      fatal_error ("Cannot resolve " ^ lid)
  in
  transl_normal_path p

(* Unique keys *)

module ExportTbl = Hashtbl.Make(struct
  type t = stype
  let equal = Internal.equal ~ignore_props:false ~ignore_path:false
  let hash = Internal.hash0
end)

let stypes_to_record = ref []
let export_tbl = ExportTbl.create 8
let nstypes_to_record = ref 0
let stypes_table_id = ref (Ident.create "stypes_table")
let normalize = Internal.normalize ~ignore_props:false ~ignore_path:false

let clear_tbl () =
  stypes_to_record := [];
  nstypes_to_record := 0;
  ExportTbl.clear export_tbl

let unique_key ty =
  let ty = normalize ty in
  try ExportTbl.find export_tbl ty
  with Not_found ->
    let id = !nstypes_to_record in
    stypes_to_record := ty :: !stypes_to_record;
    ExportTbl.replace export_tbl ty id;
    incr nstypes_to_record;
    id

let set_module_name s =
  clear_tbl ();
  stypes_table_id := Ident.create "stypes_table"

let extract_ty ty =
  Lprim(Pfield (unique_key ty), [Lvar !stypes_table_id])

let extract_cst = function
  | Lconst c -> c
  | _ -> raise Exit

let block tag l =
  try Lconst(Const_block(tag, List.map extract_cst l))
  with Exit -> Lprim(Pmakeblock(tag, Immutable), l)

let tuple l = block 0 l

let rec list f = function
  | [] -> Lconst(Const_pointer 0)
  | hd :: tl -> tuple [f hd; list f tl]

let cstr s =
  let d =
    try Env.lookup_constructor (Longident.parse s) (Env.initial_with_auto ())
    with Not_found -> assert false
  in
  let tag =
    match d.cstr_tag with
    | Cstr_constant i -> i
    | Cstr_block i -> i
    | Cstr_extension _ -> assert false
  in
  fun l ->
    assert(List.length l = d.cstr_arity);
    if d.cstr_arity = 0 then Lconst(Const_pointer tag)
    else block tag l

let cstr s =
  let f = lazy (cstr s) in
  fun l -> (Lazy.force f) l

let str s = Lconst(Const_base(Const_string(s, None)))

let dt_abstract = cstr "Mlfi_types.DT_abstract"
let dt_arrow = cstr "Mlfi_types.DT_arrow"
let dt_list = cstr "Mlfi_types.DT_list"
let dt_tuple = cstr "Mlfi_types.DT_tuple"
let dt_array = cstr "Mlfi_types.DT_array"
let dt_option = cstr "Mlfi_types.DT_option"
let dt_int = cstr "Mlfi_types.DT_int"
let dt_float = cstr "Mlfi_types.DT_float"
let dt_string = cstr "Mlfi_types.DT_string"
let dt_date = cstr "Mlfi_types.DT_date"
let dt_object = cstr "Mlfi_types.DT_object"
let dt_prop = cstr "Mlfi_types.DT_prop"

let rec mk_subst subst = function
  | DT_int -> dt_int []
  | DT_float -> dt_float []
  | DT_string -> dt_string []
  | DT_date -> dt_date []
  | t when not (Internal.has_var t) -> extract_ty t
  | DT_var i -> List.nth subst i
  | DT_abstract (s, tl) -> dt_abstract [ str s; list (mk_subst subst) tl ]
  | DT_arrow (l, t1, t2) ->
      dt_arrow  [ str l; mk_subst subst t1; mk_subst subst t2 ]
  | DT_tuple tl -> dt_tuple [ list (mk_subst subst) tl ]
  | DT_list t -> dt_list [ mk_subst subst t ]
  | DT_array t -> dt_array [ mk_subst subst t ]
  | DT_option t -> dt_option [ mk_subst subst t ]
  | DT_object l ->
      let f (s, t) = tuple [ str s; mk_subst subst t ] in
      dt_object [ list f l ]
  | DT_prop (props, t) ->
      let f (k, v) = tuple [ str k; str v ] in
      dt_prop [ list f props; mk_subst subst t ]

  | DT_node _ as t ->
      (* note: we could share the creation of the subst tuple for the (rare) case
         where several nodes require substitution. *)
      let subst = tuple subst in
      lapply
        (transl_longident "Mlfi_types.Internal.substitute")
        [subst; extract_ty t]

let transl_typeof arg =
  let (t, subst) = stype_of_type arg in
  mk_subst subst t

let use_ttype ttype =
  let t =
    match Typecore.is_ttype ttype.exp_type with
    | Some t -> t
    | None -> assert false
  in
  match t.desc with
  | Tvar _ -> U_id t.id
  | Tconstr(p, [], _) ->
      let decl =
        try Env.find_type p ttype.exp_env
        with Not_found -> errstr ("Cannot find type definition for " ^ Path.name p)
      in
      if decl.type_kind = Type_abstract && decl.type_manifest = None
      then U_abstract p
      else raise (Type_unification_error(Path.name p ^ " should be abstract."))
          (* TODO: should check that it is not a built-in type (int, ...) *)
  | _ ->
      raise (Type_unification_error "only be or closed abstract types and variables.")

let transl_use_ttype_cont ttype cont =
  let k = use_ttype ttype in
  let id = Ident.create "#ttype" in
  let x =
    let old = !use_ttypes_env in
    use_ttypes_env := (k, id) :: !use_ttypes_env;
    try_finally
      (fun () -> cont ())
      (fun () -> use_ttypes_env := old)
  in
  id, transl_exp ttype, x

let transl_use_ttype ttype e =
  let id, ttype, e = transl_use_ttype_cont ttype (fun () -> transl_exp e) in
  Llet (Strict, id, ttype, e)


let transl_use_ttype_toplevel ttype =
  let k = use_ttype ttype in
  let id = Ident.create "#ttype" in
  use_ttypes_env := (k, id) :: !use_ttypes_env;
  id, transl_exp ttype


let illegal_dyn_use loc =
  error loc Illegal_dyn_use

let add_mlfi_specific_to_module lambda =
  let lambda = match List.rev !stypes_to_record with
  | [] -> lambda
  | l ->
      (* note: the order of the list l can be different in bytecode and native code due
         do the way the toplevel structures are compiled. *)
      let export ty = Marshal.to_string (Textual.export_with_digests ty) [] in
      let tbl = export (DT_tuple l) in
      Llet(Strict, !stypes_table_id,
           lapply
             (transl_longident "Mlfi_types.Textual.import_table")
             [Lconst(Const_base(Const_string (tbl, None)))],
           lambda)
  in
  clear_tbl ();
  (* We do not clear stype2key here for the toplevel. *)
  lambda

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_dyn_use ->
      fprintf ppf
        "This primitive can only be applied to its argument"
  | Illegal_dyn_type(s, ty) ->
      fprintf ppf
        "The type@ %a@ cannot be a dynamic type (%s)" Printtyp.type_expr ty s
  | Type_unification s ->
      fprintf ppf
        "Cannot use ttype: %s." s

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
