let rec drop (k : int) (l : 'a list) : 'a list =
  match k, l with
  | i, _ when i <= 0 -> l
  | _, [] -> []
  | _, (_ :: xs) -> drop (k - 1) xs

let rec take (k : int) (l : 'a list) : 'a list =
  match k, l with
  | i, _ when i <= 0 -> []
  | _, [] -> []
  | _, (x :: xs)  -> x :: take (k - 1) xs

let split_at (n : int) (l : 'a list) : 'a list * 'a list = 
  (take n l), (drop n l)

let rec rotate_left (n : int) (l : 'a list) : 'a list =
  match n with
  | _ when n = 0 -> l
  | _ when n < 0 ->  rotate_right (-n) l
  | _ -> 
    let m : int = List.length l in
    let k : int = n mod m in
    let (l : 'a list), (r : 'a list) = split_at k l in 
    r @ l

and rotate_right (n : int) (l : 'a list) : 'a list =
  match n with
  | _ when n = 0 -> l
  | _ when n < 0 ->  rotate_left (-n) l
  | _ -> 
    let m : int = List.length l in
    let k : int = m - n mod m in
    let (l : 'a list), (r : 'a list) = split_at k l in 
    r @ l
