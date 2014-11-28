open Regexp

let _ = 
  let s = "(a|b)*abb" in
  try 
    let re, count = parse_augmented_regexp s in
    Printf.printf "%s, %d'\n" (Regexp.string_of_augmented_regexp re) count
  with 
  | Failure msg -> print_endline msg
