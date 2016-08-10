type error =
| Other of Ml_location.t
| Unclosed of Ml_location.t * string * Ml_location.t * string
| Not_expecting of Ml_location.t * string

exception Error of error

let prepare_error = function
  | Other loc ->
    Ml_location.errorf_prefixed ~loc "Syntax error"
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
    Ml_location.errorf_prefixed ~loc "Syntax error: %s not expected." nonterm

let () =
  Ml_location.register_error_of_exn (
    function
    | Error err -> Some (prepare_error err)
    | _ -> None
  )

let report_error ppf err =
  Ml_location.report_error ppf (prepare_error err)

let location_of_error = function
  | Other l -> l
  | Unclosed (_, _, l, _) -> l
  | Not_expecting (l, _) -> l
