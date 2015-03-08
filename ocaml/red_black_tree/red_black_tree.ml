module type Ordered_type_sig = sig
  type t
  val compare : t -> t -> int
end

module type Set_sig = sig
  type element
  type t

  val empty : t (*The empty set*)
  val add : t -> element -> t (*Add an element*)
  val fold : ('a -> element -> 'a) -> 'a -> t -> 'a (*Visit elements*)
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

module Set (*: SET*) = struct

  module type Ordered_type = Ordered_type_sig
  module type S = Set_sig

  module Make (Ord : Ordered_type)(* : (S with type element = Ord.t)*) = struct

    type element = Ord.t

    (*Type of a binary search tree*)
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

    let of_list (l : element list) : t =
      List.fold_left (fun s e -> add s e) empty l

    let rec fold (f : ('a -> element -> 'a)) (acc : 'a) (s : t) : 'a = 
      match s with
      | Empty -> acc
      | Node {value; left; right} ->
         fold f (fold f (f acc value) left) right

    let rec contains (s : t) (e : element) =
      match s with
      | Empty -> false
      | Node ({value; left; right} as r) ->
         let c = Ord.compare e value in
         (c = 0) || (if c < 0 then contains left e else contains right e)

    let size (s : t) = fold (fun acc _ -> acc + 1) 0 s

    let union (u : t) (v : t) = fold (fun acc e -> add acc e) u v

    let intersection (u : t) (v : t) : t =
      let f acc e = 
        if contains v e then add acc e else acc in
      fold f empty u

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

(*Prepare an [Set.Ordered_type] module to pass as argument
  to [Set.Make]*)
module Int : Set.Ordered_type with type t = int = struct 
  type t = int 
  let compare = Pervasives.compare 
end

(*Make a priority queue module*)
module Int_set = Set.Make (Int)

type 'a set = (module Set.S with type element = 'a)
