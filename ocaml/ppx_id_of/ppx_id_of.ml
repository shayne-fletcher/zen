(*A PPX syntax extension

  - The idea is to to transform declarations of the form type
  declarations of the form [type t = A [@id 1] | B of int [@id 4]
  [@@id_of]] into [type t A | B of int;; let id_of = function | A -> 1
  | B _ -> 4]];
  - Compile with [ocamlc -o ppx_id_of.exe 
    -I +compiler-libsocamlcommon.cma ppx_id_of.ml];
  - Test with [ocamlc -dsource -ppx ppx_id_of.exe prog.ml].

  I've been testing with the following program.
  {[
  type t = A [@id 2] | B of int [@id 4] [@@id_of]

  module M = struct
    type t =
    | Foo of int [@id 42]
    | Bar [@id 43] [@@id_of]

    module N = struct
      type t = Baz [@id 8] | Quux of string * int [@id 7] [@@id_of]

      module Q = struct
        type t = U [@id 0] [@@id_of]
      end
    end
  end;;
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

(*[structure_item_mapper mapper item] (when it returns), will return
  [item] unmodified however, it may replace the structure currently on
  the top of the stack by appending a new structure item i.e. the AST
  of an [id_of] function*)
let structure_item_mapper 
    (mapper : mapper) 
    (item : structure_item) : structure_item = 
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
          (*No [@@id_of] : just return the structure item*)
          | [] -> item
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
            (*Replace the structure on the top of the stack (of which
              this structure item is an element) with one extended
              with the [id_of] structure item we just synthesized*)
            Stack.push ((Stack.pop structures) @ [id_of]) structures;
            (*Finally, return this structure item (unmodified)*)
            item
        end
      (*Case the type identifier is something other than [t]*)
      | _ -> default_mapper.structure_item mapper item
    end
  (*Case this structure item is something other than a single type
    declaration*)
  | _ -> default_mapper.structure_item mapper item

(*[module_binding_mapper mapper binding] computes and returns a new
  binding [binding']. [binding] is replaced [binding'] in the
  structure on the top of the stack before returning*)
let module_binding_mapper 
    (mapper : mapper) (binding : module_binding) : module_binding =
  (*Module bindings can contain module expressions which in turn can
    contain structures*)
  (*So, a module binding built from an invocation of
    [default_mapper.module_expr] on the expression in [binding] may
    produce a new, different binding*)
  let binding' = 
    {binding with 
      pmb_expr=(default_mapper.module_expr mapper binding.pmb_expr)} in
  (*[binding] is in a structure item of the structure on the top of
    the stack*)
  let parent_structure =Stack.pop structures in
  (*Find the structure item owning [binding] and
    replace it with a new one referring to [binding'] *)
  let update = List.fold_right 
    (fun ({pstr_desc;pstr_loc} as m) acc ->
      match pstr_desc with
      | Pstr_module {pmb_name={txt;_};_} when txt = binding.pmb_name.txt ->
          {m with pstr_desc = Pstr_module binding'} :: acc
      | _ -> m :: acc
    ) parent_structure [] in
  Stack.push update structures;
  (*Now we can return this newly comptued binding*)
  binding'

(*[structure_mapper mapper structure] pushes [structure] onto the
  stack, delegates to [default_mapper] to organize for traversal over
  the [structure_item] values and finally, returns the structure on
  the top of the stack*)
let structure_mapper mapper structure =
  if (List.length structure <> 0) then
    begin
      Stack.push structure structures;
      ignore (default_mapper.structure mapper structure);
      Stack.pop structures
    end
  else default_mapper.structure mapper structure

(*[id_of_mapper argv] is a function from a [string list] (arguments)
  producing a [mapper] record with the [structure] and
  [structure_item] fields bound to the functions above*)
let id_of_mapper argv = {
  default_mapper with
    module_binding = module_binding_mapper;
    structure = structure_mapper;
    structure_item = structure_item_mapper
}

(*Register the [id_of_mapper] function with the [Ast_mapper] module
  against the symbolic name ["id_of"]*)
let () = register "id_of" id_of_mapper
