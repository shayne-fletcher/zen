type error =
| Other of Ml_location.t
| Unclosed of Ml_location.t * string * Ml_location.t * string
| Not_expecting of Ml_location.t * string

exception Error of error

let prepare_error : error -> Ml_location.error = function
  | Other loc -> Ml_location.errorf_prefixed ~loc "Syntax error"
  | Unclosed (opening_loc, opening, closing_loc, closing) ->
    Ml_location.errorf_prefixed 
      ~loc:closing_loc
      ~sub:[
        Ml_location.errorf_prefixed 
          ~loc:opening_loc
          "This '%s' might be unmatched" opening
      ]
      "Syntax error: '%s' expected" closing
  | Not_expecting (loc, nonterm) ->
    let (mk_err : string -> Ml_location.error) = 
      Ml_location.errorf_prefixed ~loc "Syntax error: %s not expected." in
    mk_err nonterm

let () =
  Ml_location.register_error_of_exn (
    function
    | Error err -> Some (prepare_error err)
    | _ -> None
  )
