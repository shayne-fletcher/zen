(*open Regexp*)

(*
let chars = Array.of_list [Some 'a'; Some 'b'; Some 'a'; Some 'b'; Some 'b'; None]
let s = Int_set.of_list [0; 1; 2]
let string_of_pair (ch, s') =
  match ch with
  | None -> Printf.sprintf "(None, %s)" (string_of_set string_of_int s')
  | Some c -> Printf.sprintf "(Some '%c', %s)" c (string_of_set string_of_int s')
let () = Printf.printf "%s" (string_of_list string_of_pair (partition chars s))
*)

let explode s =
  let n = String.length s in
  let rec loop acc i =
    if i = n then List.rev acc
    else loop (String.get s i :: acc) (i + 1) in
  loop [] 0

let implode l =
  let n = List.length l in
  let buf = Bytes.create n in
  let f i c = Bytes.set buf i c in
  List.mapi f l ; Bytes.to_string buf

type 'a recognizer =
| Remans of 'a list
| Fails


(*
let _ = 
  let s = "(a|b)*abb" in
  try 
    let dfa = dfa_of (regexp_follow s) in
    Printf.printf "%s\n" (string_of_array string_of_state dfa)
  with 
  | Failure msg -> print_endline msg

*)
