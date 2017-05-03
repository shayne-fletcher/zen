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
    (item : structure_item) : structure_item = 

  match item with
  (*Case of a module*)
  | {pstr_desc = Pstr_eval (_, _); _} ->
    (* Printf.printf "Seeing a eval structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_primitive _; _} ->
    (* Printf.printf "Seeing a primitive structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_typext _; _} ->
    (* Printf.printf "Seeing a typext structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_exception _; _} ->
    (* Printf.printf "Seeing a exception structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_module _; _} ->
    (* Printf.printf "Seeing a module structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_recmodule _; _} ->
    (* Printf.printf "Seeing a rec module structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_modtype _; _} ->
    (* Printf.printf "Seeing a module type  structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_open _; _} ->
    (* Printf.printf "Seeing an open structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_class _; _} ->
    (* Printf.printf "Seeing a class structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_class_type _; _} ->
    (* Printf.printf "Seeing a class type structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_include _; _} ->
    (* Printf.printf "Seeing an include structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_attribute _; _} ->
    (* Printf.printf "Seeing an attribute structure item\n"; *)
    default_mapper.structure_item mapper item
  | {pstr_desc = Pstr_extension (_, _); _} ->
    (* Printf.printf "Seeing an extension structure item\n"; *)
    default_mapper.structure_item mapper item

  (*Case of a single inductive type declaration*)
  | { pstr_desc = Pstr_type (_, [type_decl]); pstr_loc} ->
    begin
      (* Printf.printf "Seeing a declaration of a type 't'\n"; *)
      match type_decl with
        (*Case the type identifer is [t]*)
      | {ptype_name = {txt = "t"; _};
         ptype_kind = Ptype_variant constructor_declarations;
         ptype_attributes;
         _} ->
        (* Printf.printf "Writing an \"id_of\" for \"t\"\n"; *)
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
      | _ -> default_mapper.structure_item mapper item
    end
  (*Case this structure item is something other than a single type
    declaration*)
  | _ -> default_mapper.structure_item mapper item

let module_binding_mapper mapper binding =
  let b = 
    {binding with pmb_expr=(mapper.module_expr mapper binding.pmb_expr)} in

  let running=Stack.pop structures in
  let running' = List.fold_right 
    (fun ({pstr_desc;pstr_loc} as m) acc ->
      match pstr_desc with
      | Pstr_module {pmb_name={txt;_};_} when txt = binding.pmb_name.txt ->
          {m with pstr_desc = Pstr_module b} :: acc
      | _ -> m :: acc
    ) running [] in
  Stack.push running' structures;
  b

let module_expr_mapper mapper expr =
  let {pmod_desc;pmod_loc;pmod_attributes} = expr in
  match pmod_desc with
  | Pmod_structure s ->
    let s' = mapper.structure mapper s in
    {expr with pmod_desc=Pmod_structure s'}
  | _ -> default_mapper.module_expr mapper expr

(*[structure_mapper mapper structure] pushes [structure] onto the
  stack, delegates to the [default_mapper] to organize for traversal
  into the contained [structure_item] values and finally, returns the
  [structure] on the top of the stack*)
let structure_mapper mapper structure =
  if (List.length structure <> 0) then
    begin
      (* Printf.printf "Pushing a structure of %d items onto the stack\n" (List.length structure); *)
      Stack.push structure structures;
      ignore (default_mapper.structure mapper (Stack.top structures));
      (* Printf.printf "Popping a structure of %d items from the stack\n" (List.length (Stack.top structures)); *)
      Stack.pop structures
    end
  else 
    default_mapper.structure mapper structure

(*[id_of_mapper argv] is a function from a [string list] (arguments)
  producing a [mapper] record with the [structure] and
  [structure_item] fields bound to the functions above*)
let id_of_mapper argv = {
  default_mapper with
    module_binding = module_binding_mapper;
    module_expr = module_expr_mapper;
    structure = structure_mapper;
    structure_item = structure_item_mapper
}

(*Register the [id_of_mapper] function with the [Ast_mapper] module
  against the symbolic name ["id_of"]*)
let () = register "id_of" id_of_mapper
