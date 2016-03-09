open Lambda
open Reduce

let () = 
  let l = eval (lambda_of_string "(\\x y. x) (\\y. y)") in
  (* print_lambda l *)
  Printf.printf "%s\n" (string_of_lambda l)
