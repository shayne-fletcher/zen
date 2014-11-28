open Regexp

let partition chars s =
  let f acc c =
    match c with
    | Some _ ->
      if List.mem_assoc c acc then acc 
      else
        let f i acc = if chars.(i) <> c then acc else Int_set.add i acc in
        let s' =  Int_set.fold f s (Int_set.empty) in
        (c, s') :: acc
    | None -> if List.mem_assoc c acc then acc else (c, Int_set.empty) :: acc in
  List.rev (Array.fold_left f [] chars)
  
let chars = Array.of_list [Some 'a'; Some 'b'; Some 'a'; Some 'b'; Some 'b'; None]
let s = Int_set.of_list [0; 1; 2]
let string_of_pair (ch, s') =
  match ch with
  | None -> Printf.sprintf "(None, %s)" (string_of_set string_of_int s')
  | Some c -> Printf.sprintf "(Some '%c', %s)" c (string_of_set string_of_int s')
let () = Printf.printf "%s" (string_of_list string_of_pair (partition chars s))

(*
let _ = 
  let s = "(a|b)*abb" in
  try 
    (*
    let re, count = parse_augmented_regexp s in
    Printf.printf "%s, %d'\n" (Regexp.string_of_augmented_regexp re) count
    *)
    let e, follow, chars=regexp_follow s in
    Printf.printf "%s\n" (string_of_follow_result (e, follow, chars))
  with 
  | Failure msg -> print_endline msg
*)
