open Lambda

let print_term (t : t) : unit = 
  Printf.printf "%s" (string_of_lambda t)

let () = print_term (lambda_of_string "(\\x.x)")
