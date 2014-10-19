let range s e = 
  let rec aux acc s e = 
    if s >= e then acc
  else aux (s :: acc) (s + 1) e
  in List.rev (aux [] s e)

let _ = range 0 (2.0 ** (26 |> float_of_int) |> int_of_float)

(*
let rec loop i =
  let n = 2.0 ** (i |> float_of_int) |> int_of_float in
  try
    let _ = range 0 n in
    loop (i + 1)
  with
  | Stack_overflow -> 
      Printf.printf "Stack overflow at i = %d, n = %d\n" i n
let () = loop 0
*)
    

