type ('a, 'b) t =
| Empty
| Node of ('a , 'b) t * 'a * 'b * ('a, 'b) t * int

let empty : ('a, 'b) t = Empty

let height : ('a, 'b) t -> int = function
  | Empty -> 0
  | Node (_, _, _, _, h) -> h

let create (l : ('a, 'b) t) (x : 'a) (d : 'b) (r : ('a, 'b) t) : ('a, 'b) t =
  let hl = height l and hr = height r in
  Node (l, x, d, r, (max hl hr) + 1)

let balance (l : ('a, 'b) t) (x : 'a) (d : 'b) (r : ('a, 'b) t) : ('a, 'b) t =
  let hl = height l and hr = height r in
  if hl > hr + 1 then
    match l with
    | Node (ll, lv, ld, lr, _) when height ll >= height lr ->
    (*Case 1*)
      create ll lv ld (create lr x d r)
    | Node (ll, lv, ld, Node (lrl, lrv, lrd, lrr, _), _) ->
    (*Case 2*)
      create (create ll lv ld lrl) lrv lrd (create lrr x d r)
    | _ -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, rv, rd, rr, _) when height rr >= height rl ->
    (*Case 3*)
      create (create l x d rl) rv rd rr
    | Node (Node (rll, rlv, rld, rlr, _), rv, rd, rr, _) ->
    (*Case 4*)
      create (create l x d rll) rlv rld (create rlr rv rd rr)
    | _ -> assert false
  else
    create l x d r
  
let rec add (x : 'a) (data : 'b) : ('a, 'b) t -> ('a, 'b) t = function
    | Empty -> Node (Empty, x, data, Empty, 1)
    | Node (l, v, d, r, h) ->
      let c = compare x v in
      if c = 0 then
        Node (l, x, data, r, h)
      else if c < 0 then
        balance (add x data l) v d r
      else 
        balance l v d (add x data r)

let rec merge (l : ('a, 'b) t) (r : ('a, 'b) t) : ('a, 'b) t = 
  match (l, r) with
  | Empty, t -> t
  | t, Empty -> t
  | Node (l1, v1, d1, r1, h1), Node (l2, v2, d2, r2, h2) ->
    balance l1 v1 d1 (balance (merge r1 l2) v2 d2 r2)

let remove (id : 'a) (t : ('a, 'b) t) : ('a, 'b) t = 
  let rec remove_rec = function
    | Empty -> Empty
    | Node (l, k, d, r, _) ->
      let c = compare id k in
      if c = 0 then merge l r else
        if c < 0 then balance (remove_rec l) k d r
        else balance l k d (remove_rec r) in
  remove_rec t

let rec find (x : 'a) : ('a, 'b) t -> 'b = function
  | Empty ->  raise Not_found
  | Node (l, v, d, r, _) ->
    let c = compare x v in
    if c = 0 then d
    else find x (if c < 0 then l else r)

let rec mem (x : 'a) : ('a, 'b) t -> bool = function
  | Empty -> false
  | Node (l, v, d, r, _) ->
    let c = compare x v in
    c = 0 || mem x (if c < 0 then l else r)
    
let rec iter (f : 'a -> 'b -> unit) : ('a, 'b) t -> unit = function
  | Empty -> ()
  | Node (l, v, d, r, _) ->
    iter f l; f v d; iter f r

let rec map (f : 'a -> 'b -> 'c) : ('a, 'b) t -> ('a, 'c) t = function
  | Empty -> Empty
  | Node (l, k, d, r, h) -> 
    Node (map f l, k, f k d, map f r, h)

let rec fold (f : 'a -> 'b -> 'c -> 'c) (m : ('a, 'b) t) (acc : 'c) : 'c =
  match m with
  | Empty -> acc
  | Node (l, k, d, r, _) -> fold f r (f k d (fold f l acc))

let rec chk_bst : ('a, 'b) t -> bool = function
  | Empty -> true
  | Node (l, k, _, r, h) -> match (l, r) with
    | Empty, Empty -> true
    | Empty, Node (rl, rk, _, rr, _) -> 
      chk_bst rl && k < rk && chk_bst rr
    | Node (ll, lk, _, lr, _), Empty ->
      chk_bst ll && lk < k && chk_bst lr
    | Node (ll, lk, _, lr, _), Node (rl, rk, _, rr, _) ->
      chk_bst ll && chk_bst lr && lk < k && chk_bst rl && chk_bst rr && k < rk

let rec chk_bal : ('a, 'b) t -> bool = function
  | Empty -> true
  | Node (l, k, _, r, h) -> 
    chk_bal l && chk_bal r && h = (max (height l) (height r)) + 1

let chk_invariants = function m -> chk_bst m && chk_bal m
    
open Format

let print 
    (print_key : formatter -> 'a -> unit)
    (print_data : formatter -> 'b -> unit)
    (ppf : formatter)
    (tbl : ('a, 'b) t) : unit =
  let print_tbl ppf tbl =
    iter (fun k d -> fprintf ppf "@[<2>%a ->@ %a;@]@ " print_key k print_data d)
      tbl in
  fprintf ppf "@[<hv 2>[[%a]]@]" print_tbl tbl
