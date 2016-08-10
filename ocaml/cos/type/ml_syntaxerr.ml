type error =
| Other of Ml_location.t

exception Error of error

let prepare_error = function
  | Other loc ->
    Ml_location.errorf_prefixed ~loc "Syntax error"

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
