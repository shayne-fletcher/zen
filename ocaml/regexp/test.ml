
let _ = 
  let s = "(a|b)*abb" in
  try 
(*
    let re = Regexp.regexp_of_string s in 
    Printf.printf "%s'\n" (Regexp.string_of_regexp re)
*)
    let re = 
      Regexp.accept (Regexp.augmented_regexp (Regexp.regexp_of_string s)) in 
    Printf.printf "%s'\n" (Regexp.string_of_augmented_regexp re)
  with 
  | Failure msg -> print_endline msg
