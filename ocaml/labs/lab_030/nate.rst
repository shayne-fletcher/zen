
trials:{[n;d]
  h:0+/(n?d)=n?d;
  `hold`herr`switch`serr!(h; abs(h%n)-%d; n-h; abs(1-h%n)-(d-1)%d)}

Translated into ocaml:

let rand n m =
  let rec f acc n =
    if n <= 0 then acc else f (Random.int m::acc) (n - 1)
  in f [] n;;

let map2 f x y =
  let rec g acc x y = match x with
      []   -> acc
    | h::t -> g ((f h (List.hd y))::acc) t (List.tl y)
  in List.rev (g [] x y);;

let eq = map2 (fun a b -> a = b);;

let bsum = List.fold_left (fun x y -> x + (if y then 1 else 0)) 0;;

let trials n d = let h  = bsum (eq (rand n d) (rand n d)) in
                 let nf = float_of_int n in
                 let hf = float_of_int h in
                 let df = float_of_int d in
  Printf.printf "Num wins (hold): %d\nError %f\nNum Wins (switch): %d\nError %f\n"
                h       (abs_float (hf /. nf -. 1.0 /. df))
                (n - h) (abs_float ((1.0 -. hf /. nf) -. (df -. 1.0) /. df));;
