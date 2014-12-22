let range s e = 
  let rec aux acc s e = 
    if s >= e then acc
  else aux (s :: acc) (s + 1) e
  in List.rev (aux [] s e)

let rec drop l k =  
  if k <= 0 then l 
  else 
    match l with
    | [] -> []
    | ( _ :: tl) -> drop tl (k - 1)

let rec take l k = 
  if k <= 0 then [] 
  else 
    match l with
    | [] -> []
    | (hd :: tl) -> hd :: (take tl (k - 1))

let split l k = (take l k, drop l k)

let string_of_list f l = "[" ^ String.concat ";" (List.map f l) ^ "]" ;;

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
  List.iteri f l ; Bytes.to_string buf

let rec substrings s =
  let rec aux s = 
    let g acc elem = 
      if List.mem elem acc then acc else elem :: acc in
    let f acc i =
      let left, right = take s i, drop s (i + 1) in
      match left, right with
      | [], [] ->  acc
      | [], (_ :: _) -> List.fold_left g acc (aux right)
      | (_ :: _), [] -> List.fold_left g acc (aux left)
      | (_, _) -> List.fold_left g (List.fold_left g acc (aux left)) (aux right)  in
    List.fold_left f [s] (range 0 ((List.length s))) in
  let l = aux (explode s) in
  let f acc elem = implode elem :: acc in
  List.fold_left f [] l

type pos = {first : int; last : int}

let substrings2 t =
  let f acc i = 
    let s = drop t i in
    let f acc j = (take s j, {first=i; last=i + j - 1}) :: acc in
    acc @ (List.fold_left f [] (range 1 (List.length s + 1))) in
  List.fold_left f [] (range 0 (List.length t))

