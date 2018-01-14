type ('ty, 'v) t =
  | [] : ('v, 'v) t
  | ( :: ) : 'a * ('ty, 'v) t -> ('a -> 'ty, 'v) t

let hd = fun (type a ty v) x ->
  match x with
  | ((x : a) :: (_ : ((ty, v) t))) -> x
  | [] -> failwith "hd"

let tl = fun (type a ty v) x ->
  match x with
  | ((_ : a) :: (tl : ((ty, v) t))) -> tl
  | [] -> failwith "tl"

let rec append : type ty1 ty2 v. (ty1, ty2) t -> (ty2, v) t -> (ty1, v) t =
  fun l m ->
    match l with
    | [] -> m
    | h :: tl -> h :: (append tl m)
