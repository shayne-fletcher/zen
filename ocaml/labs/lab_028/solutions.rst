(*Joel Bjornson*)

module Tree = struct

  type ('k, 'v) t =
    | Leaf
    | Node of 'k * 'v * int * ('k,'v) t * ('k,'v) t

  let map f tree =
    let rec aux t cont =
      match t with
      | Leaf -> 
        cont Leaf
      | Node (k,v,h,l,r) -> 
        aux l @@ fun l -> aux r @@ fun r -> cont @@ Node (k, f k v, h,l,r)
    in
    aux tree (fun x -> x)

  let fold f tree zero =
    let rec aux s t cont =
      match t with
      | Leaf  ->
        cont s
      | Node (k,v,_,l,r) ->
        aux s r @@ fun s -> 
          let s = f k v s in
          aux s l cont
    in
    aux zero tree (fun x -> x)

  let height tree =
    match tree with
    | Leaf              -> 1
    | Node (_,_,h,_,_)  -> h


  let zero_balance = 0
  let low_balance = -2
  let high_balance = 2

  let node k v l r = Node (k, v, 1 + max (height l) (height r), l, r)

  let balance = function 
    | Leaf  -> zero_balance
    | Node (_,_,_,l,r) -> height l - height r

  (* Rotates to the left. Right child node will be the new root node. *)
  let rotate_left = function
    | Node (k,v,h,l, Node (rk,rv,rh,rl,rr)) ->
      node rk rv (node k v l rl) rr
    | tree->
      tree

  let rotate_right = function
    | Node (k,v,h,Node (lk, lv,lh,ll,lr), r) ->
        node lk lv ll ( node k v lr r)
    | tree ->
      tree

  let balance = function
    | Leaf  ->
      Leaf
    | Node(k,v,_,l,r) as tree ->
      match balance tree with
      | b when b <= low_balance ->
        rotate_left @@
          (* Right leaning case *)
          if balance r > zero_balance then
            (* Right-left case *)
            node k v l (rotate_right r)
          else
            (* Right-right case. *)
            tree
      | b when b >= high_balance ->
        rotate_right @@
          if balance l < zero_balance then
            (* Left-right case. *)
            node k v (rotate_left l) r
          else
            (* Left-left case. *)
            node k v l r
      | _ ->
          tree

  let add k v tree  =
    let rec aux tree cont =
      match tree with
      | Leaf                      ->
        cont @@ node k v Leaf Leaf
      | Node (k',v', h, l, r)   ->
        match compare k k' with
        | -1  ->
          aux l @@ fun l' -> node k' v' l' r |> balance |> cont
        | 0     ->
          cont @@ node k v l r
        | _     ->
          aux r @@ fun r'  -> node k' v' l r' |> balance |> cont
    in
    aux tree (fun x -> x)

  let merge l r =
    (* Removes the largest element from the tree *)
    let rem_largest tree =
      let rec aux tree cont = 
        match tree with
        | Leaf              -> 
          assert false
        | Node(k,v,_,l,Leaf) ->
          cont ((k,v), l)
        | Node (k,v,_,l,r)    ->
          aux r @@ fun (kv', r') -> 
            cont (kv', (balance @@ node k v l r'))
      in
      aux tree (fun x -> x)
    in
    (* Removes the smallest element from the tree. *)
    let rem_smallest tree =
      let rec aux tree cont=
        match tree with
        | Leaf                  ->
          assert false
        | Node (k,v,_,Leaf, r)  ->
          cont ((k,v),r)
        | Node (k,v,_,l,r)       ->
          aux l @@ fun (kv',l') -> cont (kv', balance @@ node k v l' r)
      in
      aux tree (fun x -> x)
    in
    match l, r with
    | Leaf, _   ->
      r
    | _, Leaf   ->
      l
    | l, r      ->
      if height l <= height r then
        let ((k',v'), l') = rem_largest l in
        node k' v' l' r
      else
        let ((k',v'),r') = rem_smallest r in node k' v' l r'

  let lookup key tree =
    let rec aux = function
      | Leaf -> 
        None
      | Node (k,v, _, l, r)    ->
        match compare key k with
        | -1  -> aux l
        | 0   -> Some v
        | _   -> aux r
    in
    aux tree

  let remove key tree  =
    let rec aux tree cont =
      match tree with
      | Leaf  ->
        cont Leaf
      | Node (k,v,_,l,r)  ->
        match compare key k with
        | -1    ->
          aux l @@ fun l' ->
            node k v l' r |> balance |> cont
        | 0     ->
            merge l r |> balance |> cont
        | _     ->
            aux r @@ fun r' ->
                node k v l r' |> balance |> cont
    in
    aux tree (fun x -> x)

  let of_list xs = List.fold_left (fun t (k,v) -> add k v t ) Leaf xs

end

module Table : Table_sig = struct
  module T = Tree 
  type ('a, 'b) t = ('a, 'b) Tree.t

  let empty = T.Leaf

  let add = T.add 

  let remove = T.remove

  let find k t = 
    match T.lookup k t with
    | Some x -> x
    | _     -> raise Not_found

  let mem k t = 
    match T.lookup k t with 
    | Some _  -> true
    | None    -> false 

  let iter f t = ignore @@ T.map f t

  let map f t = T.map f t

  let fold f = T.fold f

  let print fk fv f t =
    let open Format in
    let print k v =
      pp_print_string f "  " ; fk f k; pp_print_string f " , "; fv f v;
      pp_print_newline f ();
    in
    pp_print_string f "[";
    pp_print_newline f ();
    pp_open_vbox f 2;
    iter print t;
    pp_close_box f ();
    pp_print_string f "]";
    pp_print_newline f ();

end

(* Test module relying on 'CamlCheck' for generating random test data
 * in order to verify the properties.
 * See: https://bbgithub.dev.bloomberg.com/jbjornson2/CamlCheck
 * More to come on that!
 *)

module Test = struct
  open Tree
  open Bb_camlcheck

  let rec real_height = function
    | Leaf -> 0
    | Node (_,_,_,l,r) -> 1 + max (real_height l) (real_height r)

  let is_balanced = function
    | Leaf -> true
    | Node (_,_,_,l,r) ->
      abs (real_height l - real_height r) <= 2

  (* States that any tree constructred from a list of key value pairs
   * should be balanced *)
  let prop_is_balanced (xs : (int * string) list) =
    let t = of_list xs in
    is_true @@ is_balanced t

  (* Property that checks that a tree is still balanced after removing
   * any number of its keys in random order *)
  let prop_is_balanced_after_removing (n, (xs : (int * int) list)) =
    (xs <> []) ==> fun () ->
      let t = of_list xs in
      (* Select first n random keys *)
      let ks =
        let n = n mod List.length xs in
        xs
        |> List.sort (fun x y -> compare (snd x) (snd y)) 
        |> List.mapi (fun i (x,_) -> (i,x))
        |> List.filter (fun (i,x) -> i <= n) 
        |> List.map snd
      in
      let t = List.fold_left (fun t k -> remove k t) t ks in
      is_true @@ is_balanced t

  (* Verifies tht mapping with idenity should preserve the tree *)
  let prop_map_ident (xs : (int * int) list) =
    let t = of_list xs in
    map (fun _ v -> v) t === t

  (* Using CamlCheck to generate random test data for checking the properties *)
  let run_tests () = 
    check prop_is_balanced;
    check prop_is_balanced_after_removing;
    check prop_map_ident

end
