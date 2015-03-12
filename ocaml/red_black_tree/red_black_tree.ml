module type Ordered_type_sig = sig
  type t
  val compare : t -> t -> int
end

module type Set_sig = sig
  type element
  type t

  exception Empty_set

  val empty : t (*The empty set*)
  val add : t -> element -> t (*Add an element*)
  val remove : t -> element -> t (*Remove an element*)
  (* val fold : t -> 'a -> ('a -> element -> 'a)  -> 'a (\*Visit elements in order*\) *)
  val contains : t -> element -> bool (*Test for membership*)
  val size : t -> int (*Cardinality of a set*)
  val union : t -> t -> t (*The union of two sets*)
  val intersection : t -> t -> t (*The intersection of two sets*)
  val min_element : t -> element (*The minimum value of the set*)
  val max_element : t -> element (*The maximum value of the set*)
  val of_list : element list -> t (*Construct set from unordered list*)
end

module type SET = sig

  (*Input signature of the functor [Make]*)
  module type Ordered_type = Ordered_type_sig

  (*Output signature of the functor [Make]*)
  module type S = Set_sig

  (*Functor building an implementation of the set structure given a
    totally ordered type*)
  module Make : 
    functor (Ord : Ordered_type) -> S with type element = Ord.t

end

module Binary_search_tree : SET = struct

  (*A binary search tree is a binary tree with the following
   representation invariant: 

     - For every node n, every node in the left subtree of n has a
       value less than that of n and every node in the right subtree
       of n has a value more than that of n.  
  *)

  module type Ordered_type = Ordered_type_sig
  module type S = Set_sig

  module Make (Ord : Ordered_type) : (S with type element = Ord.t) = struct

    exception Empty_set

    type element = Ord.t
    type bst = Empty | Node of node
     and node = {value : element; left : bst; right : bst}

    type t = bst (*Set as a binary search tree*)

    let empty : t = Empty

    let rec add (s : t) (e : element) : t =
      match s with
      | Empty -> Node {value = e; left = Empty; right = Empty }
      | Node ({value; left; right} as r) ->
         let c = Ord.compare e value in
         if c = 0 then s else
           if c < 0 then  Node {r with left = add left e}
           else Node {r with right = add right e}

    let rec remove (s : t) (e : element) : t =
      (*[remove_first t] is [(t', v)] where [v] is the first value in
        [t] and [t'] is a binary search tree with all the elements of
        [t] except [v]*)
      let rec remove_first (s : t) : (t * element) =
        match s with
        | Empty -> raise Empty_set
        | Node {value; left=Empty; right} -> (right, value)
        | Node {left; _} -> remove_first left in 
      let rec remove_last (s : t) : (t * element) =
        match s with
        | Empty -> raise Empty_set
        | Node {value; left; right=Empty} -> (left, value)
        | Node {right; _} -> remove_last right in
      failwith "bar"

    let of_list (l : element list) : t =
      List.fold_left (fun s e -> add s e) empty l

    let rec fold (s : t) (acc : 'a) (f : ('a -> element -> 'a)) : 'a =
      match s with
      | Empty -> acc
      | Node {value; left; right} -> 
        fold right (f (fold left acc f) value) f

    let rec contains (s : t) (e : element) =
      match s with
      | Empty -> false
      | Node ({value; left; right} as r) ->
         let c = Ord.compare e value in
         (c = 0) || (if c < 0 then contains left e else contains right e)

    let size (s : t) = fold s 0 (fun acc _ -> acc + 1)

    let union (u : t) (v : t) = fold v u (fun acc e -> add acc e)

    let intersection (u : t) (v : t) : t =
      let f acc e = 
        if contains v e then add acc e else acc in
      fold u empty f

    let rec min_element (s : t) : element =
      match s with
      | Empty -> failwith "Empty"
      | Node {value; left; right} ->
         match left, right with
         | (Empty, _) -> value
         | _, _ -> min_element left

    let rec max_element (s : t) : element =
      match s with
      | Empty -> failwith "Empty"
      | Node {value; left; right} ->
         match left, right with
         | (_, Empty) -> value
         | _, _ -> max_element right

  end
end

module Red_black_tree (*: SET*) = struct

  (*A red-black tree is a binary search tree with additional
     representation invariants:

    - No red node has a red parent;

    - Every path from the root to an empty node has the same number of
      black nodes: the black height of the tree

   *)

  module type Ordered_type = Ordered_type_sig
  module type S = Set_sig

  module Make (Ord : Ordered_type)(* : (S with type element = Ord.t)*) = struct

    type element = Ord.t
    type color = Red | Black
    type tree = Empty | Node of node
       and node = { value : element; left : tree; right : tree; color : color }
    type t = tree

    let empty : t = Empty

    let rec add (s : t) (x : element) : t =
      let make_black (s : t) : t =
        match s with
        | Empty -> Empty
        | Node ({value;left;right;color} as r) -> 
           Node {r with color = Black } in

      let rotate x y z a b c d : t =
        Node { value = y; 
               left  = Node {color = Black; value = x; left = a; right = b};
               right = Node {color = Black; value = z; left = c; right = d};
               color = Red } in

      let balance (s : t) : t =
        match s with
        | (*1*) Node {color=Black; value=z;
                     left = 
                       Node {color = Red; value = y;
                             left = Node {color = Red; value = x;    
                                          left = a; right = b};
                             right = c};
                     right = d} -> 
                 rotate x y z a b c d
        | (*2*) Node {color=Black; value = z;
                     left = Node {color=Red; value = x;
                         left = a; right = Node {color=Red; value = y;
                           left = b; right = c}}; 
                     right=d} ->
                 rotate x y z a b c d
        | (*3*) Node {color=Black; value=x;
                     left = a; 
                     right = Node {color=Red; value=z;
                                   left = Node{color=Red; value=y;
                                               left=b; right= c}; 
                                   right=d}} ->
                 rotate x y z a b c d
        | (*4*) Node {color=Black;value=x;
                     left=a; 
                     right=Node{color=Red;value=y;
                                left=b; 
                                right=Node{color=Red;value=z;
                                           left=c;right=d}}} ->
                 rotate x y z a b c d
        | _ -> s in

      let rec walk (s : t) : t =
        match s with
        | Empty -> Node {color=Red; value=x; left=Empty; right=Empty}
        | Node {color; value; left; right} ->
           let cmp = compare x value in
           if cmp = 0 then s
           else if cmp < 0 then
             balance (Node {color=color;
                     value =value;
                     left = walk left;
                     right = right})
           else
             balance (Node {color = color;
                     value = value;
                     left = left;
                     right = walk right}) 
      in  make_black (walk s)

    let of_list (l : element list) : t =
      List.fold_left (fun s e -> add s e) empty l

  end
end

(*Prepare an [Set.Ordered_type] module to pass as argument
  to [Set.Make]*)
(*
module Int : Red_black_tree.Ordered_type with type t = int = struct 
  type t = int 
  let compare = Pervasives.compare 
end
*)
(*
(*Make a set*)
module Int_set = Binary_search_tree.Make (Int)

module Int_set = Red_black_tree.Make (Int)

type 'a set = (module Set.S with type element = 'a)
*)
