
open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

let test_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | [%expr [%test ] ] -> [%expr 42]
      | other -> default_mapper.expr mapper other ; }

let () = register "ppx_test" test_mapper
