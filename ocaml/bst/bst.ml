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
  val remove : t -> element -> t * element (*Remove an element*)
  val fold : t -> 'a -> ('a -> element -> 'a)  -> 'a (*Visit elements in order*)
  val contains : t -> element -> bool (*Test for membership*)
  val size : t -> int (*Cardinality of a set*)
  val union : t -> t -> t (*The union of two sets*)
  val intersection : t -> t -> t (*The intersection of two sets*)
  val min_element : t -> element (*The minimum value of the set*)
  val max_element : t -> element (*The maximum value of the set*)
  val of_list : element list -> t (*Construct set from unordered list*)

end

module type BST = sig

  (*Input signature of the functor [Make]*)
    module type Elem_type = sig
        include Ordered_type_sig
        val to_string : t -> string
    end

  (*Output signature of the functor [Make]*)
    module type S = sig
      include Set_sig
      val string_of_set : t -> string (*String representation*)
    end

  (*Functor building an implementation of the set structure given a
    totally ordered type*)
  module Make : functor (Elem : Elem_type) -> S with type element = Elem.t
end

module Binary_search_tree : BST = struct

  (*A binary search tree is a binary tree with the following
   representation invariant: 

     - For every node n, every node in the left subtree of n has a
       value less than that of n and every node in the right subtree
       of n has a value more than that of n.  
  *)

  module type Elem_type = sig
      include Ordered_type_sig
      val to_string : t -> string
    end

  module type S = sig
      include Set_sig
      val string_of_set : t -> string (*String representation*)
    end

  module Make (Elem : Elem_type) : (S with type element = Elem.t) = struct

    exception Empty_set

    type element = Elem.t
    type bst = Empty | Node of node
     and node = {value : element; left : bst; right : bst}

    type t = bst (*Set as a binary search tree*)

    let empty : t = Empty

    let rec add (s : t) (e : element) : t =
      match s with
      | Empty -> Node {value = e; left = Empty; right = Empty }
      | Node ({value; left; right} as r) ->
         let c = Elem.compare e value in
         if c = 0 then s else
           if c < 0 then  Node {r with left = add left e}
           else Node {r with right = add right e}

    let rec remove (s : t) (e : element) : (t * element) =
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
      match s with
      | Empty -> raise Empty_set
      | Node {value;left;right} ->
         let c = Elem.compare e value in
         if c = 0 then
           match (left, right) with
           | Empty, Empty -> (Empty, value)
           | (_, Empty) -> 
              let (left', prev) = remove_last left in
              (Node {value=prev;left=left';right}, value)
           | _ -> 
              let (right', next) = remove_first right in
              (Node {value=next; left; right=right'}, value)
         else 
           if c < 0 then
             let (left', x) = remove left e in
             (Node {value; left=left';right}, x)
           else (*c > 0*)
             let (right', x) = remove right e in
             (Node{value; left; right=right'}, x)

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
      | Node {value; left; right} ->
         let c = Elem.compare e value in
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

    let string_of_set (s : t) : string =
      "{"
       ^(String.concat ", " 
            (List.map (Elem.to_string) 
                 (List.rev (fold s [] (fun acc e -> e :: acc)))))
       ^"}"
  end
end
(*Test*)

module Int : Binary_search_tree.Elem_type with type t = int = 
 struct
   type t = int
   let compare = Pervasives.compare
   let to_string = string_of_int
 end

module Int_set = Binary_search_tree.Make (Int)

let (s : Int_set.t) = Int_set.of_list [3; 2; 4]
let () = Printf.printf "%s\n" (Int_set.string_of_set s)

let ((s : Int_set.t), (_ : int)) = Int_set.remove s 3
let () = Printf.printf "%s\n" (Int_set.string_of_set s)

let ((s : Int_set.t), (_ : int)) = Int_set.remove s 4
let () = Printf.printf "%s\n" (Int_set.string_of_set s)

let ((s : Int_set.t), (_ : int)) = Int_set.remove s 2
let () = Printf.printf "%s\n" (Int_set.string_of_set s)
