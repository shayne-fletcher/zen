type 'a stream =  Nil | Cons of 'a * (unit -> 'a stream)

let hd = function | Nil -> failwith "hd" | Cons (h, _) -> h
let tl = function | Nil -> failwith "tl" | Cons (_, t) -> t ()

let rec take (n : int) (lst : 'a stream) : 'a list = 
  match (n, lst) with
  | (n, _) when n < 0 -> invalid_arg "negative index in take"
  | (n, _) when n = 0 -> []
  | (_, Nil) -> []
  | (n, Cons (h, t)) -> h :: (take (n - 1) (t ()))

let sqrt2 = 
  let approximations = 
    let f = fun x -> x *. x -. 2.0 in
    let f' = fun x -> 2.0 *. x in
    let rec update (x : float) : float stream = 
      Cons (x, fun () -> update (x -. ((f x) /. (f' x)))) in
    update 1.0 in
List.nth (take 10 approximations) 9

(*
# List.nth (take 10 sqrt_two) 9 ;;
- : float = 1.41421356237309515
*)
