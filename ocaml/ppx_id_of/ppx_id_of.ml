(*A PPX syntax extension

  - The idea is to to transform declarations of the form type
  declarations of the form [type t = A [@id 1] | B of int [@id 4]
  [@@id_of]] into [type t A | B of int;; let id_of = function | A -> 1
  | B _ -> 4]];
  - Compile with [ocamlc -o ppx_id_of2.exe 
    -I +compiler-libsocamlcommon.cma ppx_id_of2.ml];
  - Test with [ocamlc -dsource -ppx ppx_id_of.exe test.ml].
*)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(*[case (decl, num)] produces a [Parsetree.case] from a constructor
  declaration and its code*)
let case : constructor_declaration * string -> case = function
  | ({pcd_name={txt; loc};pcd_args; pcd_attributes; _}, id) ->
    Exp.case 
      (Pat.construct 
         {txt=Lident txt; loc=(!default_loc)}  
         (match pcd_args with 
         | Pcstr_tuple [] -> None | _ -> Some (Pat.any ()))
      )
      (Exp.constant (Pconst_integer (id, None)))

(*[case_of_construct_declaration] makes a [Parsetree.case] from a
  value of [Parsetree.constructor_declaration]*)
let case_of_constructor_declaration : 
    constructor_declaration -> case =  function
  | {pcd_name={loc; _}; pcd_attributes; _} as decl ->
    match List.filter (fun ({txt;_}, _) -> txt="id") pcd_attributes with
    (*No "@id"*)
    | [] -> raise (Location.Error (Location.error ~loc "[@id] : Missing"))
    (*Single "@id"*)
    | [(_, payload)] -> 
      begin match payload with 
        | PStr [{pstr_desc=Pstr_eval ({pexp_desc=
            Pexp_constant (Pconst_integer (id, None)); _}, _)
          }] -> case (decl, id)
        | _ -> 
          raise (Location.Error (Location.error ~loc 
          "[@id] : Bad (or missing) argument (should be int e.g. [@id 4])"))
      end
    (*Many "@id"s*)
    | (_ :: _) -> raise (Location.Error (Location.error ~loc 
                  "[@id] : Multiple occurences"))

(*[eval_structure_item mapper item acc] computes structure items to
  push on the front of [acc]. If [item] is a single declaration of an
  inductive type [t] attributed with [@@id_of], then two structure
  items will be produced : one for [t] and one synthesized for [t]'s
  [of_id] function. In all other cases, just one structure item will
  be pushed onto [acc].*)
let eval_structure_item
    (mapper : mapper) 
    (item : structure_item) 
    (acc : structure) : structure =
  match item with
  (*Case of a single inductive type declaration*)
  | { pstr_desc = Pstr_type (_, [type_decl]); pstr_loc} ->
    begin
      match type_decl with
      (*Case where the type identifer is [t]*)
      | {ptype_name = {txt = "t"; _};
         ptype_kind = Ptype_variant constructor_declarations;
         ptype_attributes;
         _} ->
        begin
          match List.filter (fun ({txt;_},_) ->txt="id_of") ptype_attributes with

          (*No [@@id_of]*)
          | [] -> default_mapper.structure_item mapper item :: acc

          (*At least one [@@id_of] (treat multiple occurences as if
            one)*)
          | _ ->
            (*Cases of an [id_of] function for [t], one for each
              of its constructors*)
            let cases= 
              List.fold_right 
                (fun x acc -> 
                  case_of_constructor_declaration x :: acc) 
                constructor_declarations [] in
            (*The [id_of] function itself*)
            let id_of : structure_item = 
              Str.value Nonrecursive [
                Vb.mk 
                  (Pat.var {txt="id_of"; loc=(!default_loc)}) 
                  (Exp.function_ cases)] in

            default_mapper.structure_item mapper item :: id_of :: acc
        end
      (*Case the type identifier is something other than [t]*)
      | _ -> default_mapper.structure_item mapper item :: acc
    end
  (*Case this structure item is something other than a single type
    declaration*)
  | _ -> default_mapper.structure_item mapper item :: acc

(*[structure_mapper mapper structure] folds [eval_structure_item
  mapper] over [structure]*)
let structure_mapper 
    (mapper : mapper) 
    (structure : structure) : structure =
  List.fold_right (eval_structure_item mapper) structure []

(*[type_declaration_mapper mapper decl] computes a new
  [type_declaration] as [decl] stripped of [@@id_of] attributes*)
let type_declaration_mapper mapper decl =
  match decl with
    (*Case of an inductive type "t"*)
  | {ptype_name = {txt = "t"; _};
     ptype_kind = Ptype_variant constructor_declarations;
     ptype_attributes;
     _} ->
    let (_, attrs) = 
      List.partition (fun ({txt;_},_) ->txt="id_of") ptype_attributes in
    {(default_mapper.type_declaration mapper decl) 
    with ptype_attributes=attrs}
  (*Not an inductive type named "t"*)
  | _ -> default_mapper.type_declaration mapper decl

(*[constructor_declaration_mapper mapper decl] computes a new
  [constructor_declaration] as [decl] stripped of [@id] attributes*)
let constructor_declaration_mapper mapper decl =
  match decl with
  | {pcd_name={loc; _}; pcd_attributes; _} ->
    let (_, attrs) = 
      List.partition (fun ({txt;_}, _) -> txt="id") pcd_attributes  in
    {(default_mapper.constructor_declaration mapper decl) 
      with pcd_attributes=attrs}

(*[id_of_mapper argv] is a function from a [string list] (arguments)
  producing a [mapper] record with [structure], [type_declaration] and
  [constructor_declaration] fields bound to the functions above*)
let id_of_mapper argv = {
  default_mapper with
    structure = structure_mapper;
    type_declaration = type_declaration_mapper;
    constructor_declaration = constructor_declaration_mapper
}

(*Register the [id_of_mapper] function with the [Ast_mapper] module
  against the symbolic name ["id_of"]*)
let () = register "id_of" id_of_mapper
