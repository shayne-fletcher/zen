(*{{:http://courses.cms.caltech.edu/cs11/material/ocaml/lab4/lab4.html}
   Caltech}*)

(**Priority queues over ordered types*)
module type PRIORITY_QUEUE = sig

    (**Output signature of the functor [Priority_queue.Make]*)
    module type S = sig
        exception Empty

        type element (*Abstract type of elements of the queue*)
        type t (*Abstract type of a queue*)

        val empty : t (*The empty queue*)
        val is_empty : t -> bool (*Check if queue is empty*)
        val insert : t -> element -> t (*Insert item into queue*)
        val delete_min : t -> t (*Delete the minimum element*)
        val find_min : t -> element (*Return the minimum element*)
        val of_list : element list -> t
      end

    (**Input signature of the function [Priority_queue.Make]*)
    module type Ordered_type = sig
        type t
        val compare : t -> t -> int
      end

   (**Functor building an implementation of the priority queue struture
      given a totally ordered type*)
    module Make : functor (Ord : Ordered_type) -> S with type element = Ord.t
  end

let rec take (k : int) (l : 'a list) : 'a list =
  match (k, l) with
  | n, _ when n <= 0 -> []
  | _, [] -> []
  | n, (x :: xs) -> x :: take (n - 1) xs

let rec drop (k : int)  (l : 'a list) : 'a list =
  match (k, l) with
  | n, xs when n <= 0 -> xs
  | _, [] -> []
  | n, (_ :: xs) -> drop (n - 1) xs

let slice (l : 'a list) (i : int) (j : int) = take (j - i) (drop i l)

module Priority_queue : PRIORITY_QUEUE = struct 

  module type S = sig
      exception Empty

      type element (*Abstract type of elements of the queue*)
      type t (*Abstract type of a queue*)

      val empty : t (*The empty queue*)
      val is_empty : t -> bool (*Check if queue is empty*)
      val insert : t -> element -> t (*Insert item into queue*)
      val delete_min : t -> t (*Delete the minimum element*)
      val find_min : t -> element (*Return the minimum element*)
      val of_list : element list -> t
    end

  module type Ordered_type = sig
    type t
    val compare : t -> t -> int
  end

  module Make (Elt : Ordered_type) : (S with type element = Elt.t) = struct

    exception Empty

    type element = Elt.t
    type heap = Leaf | Node of (element * int * heap * heap) (*Lefist heap*)
    type t = heap

    let empty : heap = Leaf
    let is_empty : heap -> bool = function | Leaf -> true | _ -> false

    let rank : heap -> int = function | Leaf -> 0 | Node (_, r, _, _) -> r

    (*Here is how to make a new heap from a minimum element and two
      heaps: the resulting heap must have
      - The given minimum element;
      - A rank which is the smaller of the ranks of the original heaps
        plus 1;
      - A left subheap which is the original heap with the larger rank;
      - A right subheap which is the original heap with the smaller rank.    
      This algorithm will preserve the leftist heap property in the
      merged heap.
    *)
    let new_heap (e : element) (l : heap) (r : heap) =
      let left, right = if rank l > rank r then (l, r) else (r, l) in
      Node (e, ((min (rank l) (rank r) ) + 1), left, right)

    (*Algorithm to merge two leftist heaps to create a new leftist heap:
      - If either heap is empty, return the other heap;
      - If the first heap's minimum element is smaller than the second
        heap's minimum element, make a new heap (see above) from the
        first heap's minimum element, the first heap's left subheap, and
        the result of merging the first heap's right subheap with the second
        heap;
      - Otherwise make a new heap (see above) from the second heap's
        minimum element, the second heap's left subheap, and the result
        of merging the first heap with the second heap's right subheap.
    *)
    let rec merge (x : heap) (y : heap) : heap = 
      match (x, y) with
      | Leaf, Leaf -> Leaf
      | (Leaf, (Node (_, _, _, _) as n)) -> n
      | ((Node (_, _, _, _) as n), Leaf) -> n
      | ((Node (e1, k1, l1, r1) as h1), (Node (e2, k2, l2, r2) as h2)) ->
        if Elt.compare e1 e2 = (-1) 
        then new_heap e1 l1 (merge r1 h2) 
        else new_heap e2 l2 (merge h1 r2)

    let insert (x : heap) (e : element) = merge x (Node (e, 0, Leaf, Leaf))

    let delete_min (x : t) : t = 
      match x with
      | Leaf -> raise Empty
      | Node (_, _, l, r) -> merge l r

    let find_min (x : heap) : element =
      match x with
      | Leaf -> raise Empty
      | Node (e, _, _, _) -> e

    let rec of_list (l : element list) =
      match l with
      | [] -> Leaf
      | [h] -> insert Leaf h
      | _ ->
        let mid = (List.length l)/2 in
        merge (of_list (slice l 0 mid)) (of_list (slice l mid (List.length l)))
    (*{{:http://bartoszmilewski.com/2014/01/21/functional-data-structures-in-c-leftist-heaps/}
       Funcational Data Structures in C++ - Bartosz Milewski}*)

  end
end

(*Type abbreviations*)
type 'a order_impl = (module Priority_queue.Ordered_type with type t = 'a)
type 'a queue_impl = (module Priority_queue.S with type element = 'a)

(*[heap_sort] takes a module as a first class value and uses a localy
  abstract type to connect it with the list element type*)
let heap_sort (type a) (queue : a queue_impl) (l : a list) : a list =
  let module Queue = (val queue : Priority_queue.S with type element = a) in
  let rec loop acc h =
    if Queue.is_empty h then acc
    else
      let p = Queue.find_min h in
      loop (p :: acc) (Queue.delete_min h) in
  List.rev (loop [] (Queue.of_list l))

(*Test*)

(*Prepare an [Priority_queue.Ordered_type] module to pass as argument
  to [Priority_queue.Make]*)
module Int : Priority_queue.Ordered_type with type t = int = struct 
  type t = int let compare = Pervasives.compare 
end

(*Make a priority queue module*)
module Int_prioqueue : (Priority_queue.S with type element = int) = Priority_queue.Make (Int)

(*Make a first class value of the module by packing it*)
let queue = (module Int_prioqueue : Priority_queue.S with type element = int)

(*Now, pass the module to [heap_sort]*)
let sorted = heap_sort queue [-1; -2; 2] (*Produces the list [-2; -1; 2]*)

(*Functions to create queue modules*)
let mk_ord : 'a. unit -> 'a order_impl =
  fun (type s) () ->
    (module 
     struct 
       type t = s 
       let compare = Pervasives.compare 
     end : Priority_queue.Ordered_type with type t = s
    )

let mk_queue : 'a. 'a order_impl -> 'a queue_impl =
  fun (type s) ord ->
    let module Ord = (val ord : Priority_queue.Ordered_type with type t = s) in
    (module Priority_queue.Make (Ord) : Priority_queue.S with type element = s)

let mk_queue2 : 'a. unit -> 'a queue_impl =
  fun (type s) ord ->
    let module Ord = (val mk_ord () : Priority_queue.Ordered_type with type t = s) in
    (module Priority_queue.Make (Ord) : Priority_queue.S with type element = s)

(*For example,
    # let s : int queue_impl = mk_queue (mk_ord ());;
    val s : int queue_impl = <module>

  # heap_sort (mk_queue2 ()) [-3; 1; 5] ;;
  - : int list = [-3; 1; 5]

*)
