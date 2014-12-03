let string_of_set f fold s =
  let f i acc = (f i) :: acc in
  "[" ^ String.concat "," (List.rev (fold f s [])) ^ "]"

let string_of_list f l =
  "[" ^ String.concat ";" (List.map f l) ^ "]"

let string_of_array f arr =
  "[|" ^ String.concat ";" (List.map f (Array.to_list arr)) ^ "|]"
