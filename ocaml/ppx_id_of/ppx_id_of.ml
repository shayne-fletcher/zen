(*This is a PPX syntax extension.

  - The idea is to to transform declarations of the form type
  declarations of the form [type t = A [@id 1] | B of int [@id 4]
  [@@id_of]] into [type t A | B of int;; let id_of = function | A -> 1
  | B _ -> 4]];
  - Compile with [ocamlc -o ppx_id_of.exe 
    -I +compiler-libsocamlcommon.cma ppx_id_of.ml];
  - Test with [ocamlc -dsource -ppx ppx_id_of.exe prog.ml].
*)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(*Initit a stack of [Parsetree.structure]s*)
let structures : structure Stack.t = Stack.create ()

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
  | {pcd_name={txt;loc};pcd_attributes; _} as decl ->
    let (id_attrs, rest) = 
      List.partition (fun ({txt;_}, d) -> txt="id") pcd_attributes in
    match id_attrs with
    | [(_, p)] -> 
      begin
        match p with 
        | PStr [{pstr_desc=Pstr_eval ({pexp_desc=
            Pexp_constant (Pconst_integer (id, None)); _}, _)
          }] -> case (decl, id)
        | _ -> 
          raise (Location.Error (
            Location.error ~loc 
              "[@id] : Bad (or missing) argument (should be int e.g. [@id 4])")
          )
      end
    | [] -> 
      raise (Location.Error (Location.error ~loc  "[@id] : Missing"))
    | (_ :: _) ->
      raise (Location.Error (Location.error ~loc "[@id] : Multiple occurences"))

(*[structure_item_mapper mapper item] (when it returns), will return
  [item] unmodified however, it may replace the structure currently on
  the top of the stack by appending a new [structure_item] that is the
  AST of an [id_of] function*)
let structure_item_mapper 
    (mapper : mapper) 
    (item : structure_item) : structure_item= 
  match item with
  (*Case of a single inductive type declaration*)
  | { pstr_desc = Pstr_type (_, [type_decl]); pstr_loc} ->
    begin
      match type_decl with
        (*Case the type identifer is [t]*)
      | {ptype_name = {txt = "t"; _};
         ptype_kind = Ptype_variant constructor_declarations;
         ptype_attributes;
         _} ->
        let (id_of_attrs, rest) =
          List.partition (fun ({txt;_},_) ->txt="id_of") ptype_attributes in
        begin
          match id_of_attrs with
          (*No [@@id_of] : just return the structure item*)
          | [] -> item

          (*At least one occurence of [@@id_of] (we treat multiple
            occurences as if there was just one)*)
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
            (*Pop the structure containing [item] from the top of
              stack*)
            let structure = Stack.pop structures in
            (*Produce a new structure extending the original with the
              [id_of] function and push it onto the stack*)
            Stack.push (structure @ [id_of]) structures;
            (*Finally, return this structure item (unmodified)*)
            item
        end
      (*Case the type identifier is something other than [t]*)
      | _ -> item
    end
  (*Case this structure item is something other than a single type
    declaration*)
  | _ -> item

(*[structure_mapper mapper structure] pushes [structure] onto the
  stack, delegates to the [default_mapper] to organize for traversal
  into the contained [structure_item] values and finally, returns the
  [structure] on the top of the stack*)
let structure_mapper mapper structure =
  Stack.push structure structures;
  ignore (default_mapper.structure mapper (Stack.top structures));
  Stack.pop structures

(*[id_of_mapper argv] is a function from a [string list] (arguments)
  producing a [mapper] record with the [structure] and
  [structure_item] fields bound to the functions above*)
let id_of_mapper argv = {
  default_mapper with
    structure = structure_mapper;
    structure_item = structure_item_mapper
}

(*Register the [id_of_mapper] function with the [Ast_mapper] module
  against the symbolic name ["id_of"]*)
let () = register "id_of" id_of_mapper
