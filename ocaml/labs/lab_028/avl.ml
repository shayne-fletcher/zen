(** Author: Peter Nguyen (R&D)
 *  Date: August 9, 2016
 *  AVL tree implementation of a dictionary/association table *)
 
(** Our type definition: key * val * left subtree * right subtree * height **)
type ('a, 'b) t = Empty | Node of 'a * 'b * ('a, 'b) t * ('a, 'b) t * int

(*** HELPER FUNCTIONS ***)

(* function composition operator *)
let (<.>) f g = fun x -> f @@ g x

(* Misc tree stuff *)
let height_tree = function Empty -> 0 | Node(_,_,_,_,h) -> h
let balance_factor = function 
| Empty -> 0
| Node(_,_,l,r,_) -> height_tree r - height_tree l

let update_height = function 
| Node(k,v,l,r,_) -> Node(k,v,l,r,1 + max (height_tree l) (height_tree r))
| Empty -> Empty

(** ROTATIONS **)
let left_rot = function
| Node(k,v,l,Node(k',v',l',r',h'),h) -> update_height @@ Node(k',v', 
                                                              update_height @@ Node(k,v,l,l',h), 
                                                              r', h')
| _ -> failwith "left_rot failure"

let right_rot = function
| Node(k,v,Node(k',v',l',r',h'),r,h) -> update_height @@ Node(k',v',
                                                              l',
                                                              update_height @@ Node(k,v,r',r,h),
                                                              h')
| _ -> failwith "right_rot failure"

let left_right_rot = function
| Node(k,v,l,r,h) -> let t = update_height @@ Node(k,v,left_rot l,r,h) in right_rot t 
| _ -> failwith "left_right_rot failure"

let right_left_rot = function
| Node(k,v,l,r,h) -> let t = update_height @@ Node(k,v,l,right_rot r,h) in left_rot t 
| _ -> failwith "right_left_rot failure"

(* DRIVER FOR ROTATION FUNCTIONS *)
let rebalance = function
| Empty -> Empty
| Node(_,_,l,r,_) as t -> (if balance_factor t >= 2 then
                            (if balance_factor r >= 0 
                             then left_rot        (* Left rot *)
                             else right_left_rot) (* Right-Left rot *)
                          else if balance_factor t <= -2 then
                            (if balance_factor l <= 0 
                             then right_rot       (* Right rot *)
                             else left_right_rot) (* Left-Right rot *)
                          else fun x -> x) t

(* 'PRINT' HELPERS *)
let rec printTabs = function
| 0 -> ()
| n -> Format.print_char '\t'; printTabs @@ n - 1

(* A couple notes about 'print':
 * 0. It prints out the tree structure of the dictionary (for debug purposes)
 * 1. The tree prints sideways, with the root at the left margin, and growing
 *    to the right
 * 2. The left branches are printed out before the right branches, meaning
 *    that the left branches will sit *above* the right branches
 * 3. Each node is printed out as <key>/<value>/<height> (height printed out for
 *    debugging purposes)
 *
 *    Ex: Adding the elements (1 -> 'a'), (2 -> 'b'), and (3, 'c') in this order
 *        yields the following output when printed (asterisks represent EMPTY):
 *
 *                                  * 
 *                         1/'a'/1  
 *                                  *
 *              2/'b'/2     
 *                                  *
 *                         3/'c'/1
 *                                  *
 ***)
let rec print' pp_key pp_val ppf h = Format.(function
| Empty                -> printTabs h; print_string "*\n" 
| Node (k, v, l, r, d) -> print' pp_key pp_val ppf (h+1) l; 
                          printTabs h; Format.(fprintf ppf "%a/%a/%d\n" pp_key k pp_val v d);
                          print' pp_key pp_val ppf (h+1) r)

(* 'REMOVE' HELPERS *)
let rec drill_left = function
| Empty -> raise Not_found
| Node(k,v,l,_,_) -> match l with Empty -> k,v | _ -> drill_left l

let get_successor = function 
| Empty -> raise Not_found
| Node(_,_,_,r,_) -> drill_left r 

(*** END HELPERS ***)

(*** DEFINITION OF INTERFACE VALUES & FUNCTIONS ***)
let empty = Empty

(* Using Pervasives.compare:
   compare a b = -1 => a < b
   compare a b =  1 => a > b 
   compare a b =  0 -> a = b *)
let rec add k v = function
| Empty -> Node(k,v,Empty,Empty,1)
| Node(k',v',l,r,h) -> let comp = compare k k' in
                       if comp = 0 then Node(k,v,l,r,h)
                       else (rebalance <.> update_height) 
                            (if comp = 1 then Node(k',v', l, add k v r, h)
                             else Node(k',v', add k v l, r, h))

let rec remove k  = function
| Empty -> Empty
| Node(k',v,l,r,h) as t -> let comp = compare k k' in (rebalance <.> update_height) 
        (if      comp = -1 then Node(k',v,remove k l,r,h)
         else if comp =  1 then Node(k',v,l,remove k r,h)
         else match l,r with
         | Empty, Empty -> Empty
         | Empty, r     -> r
         | l, Empty     -> l
         | l,r          -> let k'',v'' = get_successor t in
                           Node(k'',v'',l,remove k'' r,h))

let rec find x = function
| Empty -> raise Not_found
| Node (k, v, l, r, _) -> let comp = compare x k in
                          if comp = 0 then v
                          else find x (if comp = 1 then r else l)

let rec mem x = function
| Empty -> false
| Node(k,v,l,r,_) -> let comp = compare x k in 
                     if comp = 0 then true 
                     else mem x (if comp = 1 then r else l)

(* In order traversal *)
let rec iter f = function
| Empty -> ()
| Node(k,v,l,r,_) -> iter f l; f k v; iter f r

let rec map f = function
| Empty -> Empty
| Node(k,v,l,r,h) -> Node(k,f k v, map f l, map f r, h)

(* In order traversal *)
let rec fold f t c = match t with
| Empty -> c
| Node(k,v,l,r,_) -> let c'  = fold f l c in
                     let c'' = f k v c' in
                     fold f r c''

let print pp_key pp_val ppf t = Format.open_box 0; 
                                print' pp_key pp_val ppf 0 t;
                                Format.close_box ()

