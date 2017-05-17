exception Fatal_error

(*Print a fatal error message to [stderr] then raise [Fatal_error]*)
let fatal_error msg =
  prerr_string ">> Fatal eror: "; prerr_endline msg; raise Fatal_error

(*Like [fprintf] but instead of printing on a formatter, returns a
  string containing the result of formating the arguments which is then
  passed to [fatal_erro]*)
let fatal_errorf fmt = Format.kasprintf fatal_error fmt
