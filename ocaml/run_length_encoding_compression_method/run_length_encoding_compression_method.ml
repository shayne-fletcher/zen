module F = struct

  type 'a t = | S of 'a | C of int * 'a

  let rle (bytes : 'a list) : 'a t list =
    let f (acc : 'a t list) (b : 'a) : 'a t list =
      match acc with
      | ((S e) :: tl) when e = b -> (C (2, e)) :: tl
      | ((C (n, e)) :: tl) when e = b -> (C (n + 1, b)) :: tl
      | _-> S b :: acc
    in List.rev (List.fold_left f [] bytes)

  let rld (data : ('a t) list) =
    let rec aux (acc : 'a list) (b : 'a) : (int -> 'a list) = 
      function | 0 -> acc | i -> aux (b :: acc) b (i - 1) in
    let f (acc : 'a list) (e : 'a t) : 'a list =
      acc @ (match e with | S b -> [b]| C (n, b) -> aux [] b n) in
    List.fold_left f [] data

end

module E = struct

let rle (x : 'a list) : (int * 'a) list =
  let f (acc : (int * 'a) list) (c : 'a) : (int * 'a) list =
    match acc with
    | ((n, e) :: tl) when e = c -> (n + 1, c):: tl
    | _-> (1, c) :: acc
  in List.rev (List.fold_left f [] x)

let decode (data : (int * 'a) list) =
  let repeat ((n : int), (c : 'a)) : 'a list =
    let rec aux acc i = if i = 0 then acc else aux (c :: acc) (i - 1) in
    aux [] n in
  let f (acc : 'a list) (elem : (int * 'a)) : 'a list =
    acc @ (repeat elem) in
  List.fold_left f [] data

end

module B = struct

let pack (x : char list) : char list list =
  let f (acc : char list list) (c : char) : char list list =
    match acc with
    | (((b :: _) as hd) :: tl) when c = b -> (c :: hd) :: tl
    |  _ -> [c] :: acc
  in List.fold_left f [] x

let encode (x : char list list) : (int * char) list =
  let f (acc : (int * char) list) (l : char list) : (int * char) list =
    (List.length l, List.hd l) :: acc
  in List.fold_left f [] x
  
let decode (data : (int * char) list) =
  let repeat ((n : int), (c : char)) : char list =
    let rec aux acc i = if i = 0 then acc else aux (c :: acc) (i - 1) in
    aux [] n in
  let f (acc : char list) (elem : (int * char)) : char list =
    acc @ (repeat elem) in
  List.fold_left f [] data
  
end

