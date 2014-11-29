open Regexp

(*
let chars = Array.of_list [Some 'a'; Some 'b'; Some 'a'; Some 'b'; Some 'b'; None]
let s = Int_set.of_list [0; 1; 2]
let string_of_pair (ch, s') =
  match ch with
  | None -> Printf.sprintf "(None, %s)" (string_of_set string_of_int s')
  | Some c -> Printf.sprintf "(Some '%c', %s)" c (string_of_set string_of_int s')
let () = Printf.printf "%s" (string_of_list string_of_pair (partition chars s))
*)

let _ = 
  let s = "(a|b)*abb" in
  try 
    let dfa = dfa_of (regexp_follow s) in
    Printf.printf "%s\n" (string_of_array string_of_state dfa)
  with 
  | Failure msg -> print_endline msg

