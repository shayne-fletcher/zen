let rec drop (t : 'a list) (k : int) : 'a list =  
  if k <= 0 then t 
  else 
    if List.length t = 0 then 
      []
    else drop (List.tl t) (k - 1)

let rec take (t : 'a list) (k : int) : 'a list = 
  if k <= 0 then []
  else 
    if List.length t = 0 then 
      []
    else  
      List.hd t :: take (List.tl t) (k - 1)

let split (t : 'a list) (k : int) : 'a list * 'a list = (take t k, drop t k)

let explode (s : string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1)(s.[i] :: l) in
  exp (String.length s - 1) []

let implode (l : char list) : string = 
  let res : Bytes.t= Bytes.create (List.length l) in
  let rec imp i = 
    fun l ->
      match l with
        | [] -> res
        | (c :: l) -> res.[i] <- c; imp (i + 1) l in imp 0 l

let rec scanl (f : 'b -> 'a -> 'b) (z : 'b) (xs : 'a list) : 'b list =
  z :: match xs with
  | [] -> []
  | (x :: xs) -> scanl f (f z x) xs

(*
let inits (xs : char list) : char list list = scanl (fun x a -> x @ [a]) [] xs
*)

let inits (xs : char list) : char list list = 
  let f (h :: tl) c = (c :: h) :: h :: tl in
  List.rev @@ List.map (List.rev) (List.fold_left f [[]] xs)

let rec tails xs =
  xs :: match xs with
  | [] -> []
  | (_ :: t) -> tails t

let rec prefix (us : 'a list) (vs : 'a list) : bool =
  match us, vs with
  | [], _ -> true
  | (u :: us), [] -> false
  | (u :: us), (v :: vs) -> u = v && prefix us vs

let endswith (us : 'a list) (vs : 'a list) : bool =  prefix (List.rev us) (List.rev vs)
(*let endswith (ws : char list ) (s : char list) : bool = List.mem ws (tails s)*)

(*
let matches (ws : string) (l : string) : int list = 
  List.map (List.length) 
     (List.filter (endswith (explode ws)) (inits (explode l)))
*)

let fork ((f : 'a -> 'b), (p : 'a -> 'c)) (x : 'a) : 'b * 'c = (f x, p x)

(*
let matches (ws : char list) (l : char list) : int list = 
  let sw = List.rev ws in
  List.map fst (List.filter (fun p -> prefix sw (snd p)) (List.map (fork (List.length, List.rev))(inits l)))
*)

let matches (ws : string) (s : string) : int list =
  let matches (ws : char list) (l : char list) : int list = 
    let sw = List.rev ws in
    let step (n, sx) x = (n + 1, x :: sx) in
    List.map fst (List.filter (fun p -> prefix sw (snd p)) (scanl step (0, []) l)) in
  matches (explode ws) (explode s)

let n = matches "abcab" "ababcabcab"

(*
let add x y = x + y
let mul x y = x * y
let fold_left = List.fold_left
let _ = fork (fold_left add 0, fold_left mul 1) [1; 2; 3; 4]
let _ = fold_left (fun (a, b) x -> (add a x, mul b x)) (0, 1) [1; 2; 3; 4]
*)
