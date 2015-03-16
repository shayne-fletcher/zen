(**Doubly linked, circular list. This implementation admits the empty
   list.  *)
module type Doubly_linked_list_sig = 
  sig

    type 'a cell = {
      mutable value : 'a ;
      mutable next : 'a cell ;
      mutable prev : 'a cell ;
    }

    type 'a t = 'a cell option ref

    val create : unit -> 'a t
    val singleton : 'a -> 'a t
    val of_list : 'a list -> 'a t

    val is_empty : 'a t -> bool
    val length : 'a t -> int

    val first : 'a t -> 'a cell option
    val next : 'a cell -> 'a cell
    val prev : 'a cell -> 'a cell
    val get : 'a cell -> 'a

    val set : 'a cell -> 'a -> 'a cell
    val push_front : 'a t -> 'a -> 'a cell
    val push_back : 'a t -> 'a -> 'a cell

    val insert_before : 'a cell -> 'a -> 'a cell
    val insert_after : 'a cell -> 'a -> 'a cell

end

module Doubly_linked_list : Doubly_linked_list_sig = struct

    type 'a cell = {
      mutable value : 'a ;
      mutable next : 'a cell ;
      mutable prev : 'a cell ;
    }

    type 'a t = 'a cell option ref

    let create () : 'a t = ref None

    let singleton (x : 'a) : 'a t =
      let rec nn = {value = x; next = nn; prev = nn } in
      ref (Some nn)

    let length (l : 'a t) : int =
      match !l with
      | None -> 0
      | Some node ->
         let rec loop acc n =
           if n == node then acc
           else
             loop (acc + 1) n.next
         in loop 1 node.next

    let push_front (l : 'a t) (x : 'a) : 'a cell =
      match !l with
      | None -> 
         let rec nn = {value = x; next = nn; prev = nn } in
         l := Some nn ;
         nn
      | Some node ->
         let nn = { value = x; next = node; prev = node.prev; } in
         node.prev.next <- nn;
         node.prev <- nn;
         l := Some nn ;
         nn

    let push_back (l : 'a t) (x : 'a) : 'a cell =
      match !l with
      | None -> 
         let rec nn = {value = x; next = nn; prev = nn } in
         l := Some nn ;
         nn
      | Some node ->
         let nn = { value = x; next = node; prev = node.prev; } in
         node.prev.next <- nn;
         node.prev <- nn;
         nn

    let insert_before (node : 'a cell) (x : 'a) : 'a cell =
      let nn = { value = x; next = node; prev = node.prev; } in
      node.prev.next <- nn;
      node.prev <- nn;
      nn

    let insert_after (node : 'a cell) (x : 'a) : 'a cell =
      let nn = { value = x; next = node.next; prev = node } in
      node.next.prev <- nn;
      node.next <- nn;
      nn
        
    let first (l : 'a t) : 'a cell option = !l
    let is_empty (l : 'a t) : bool = !l = None

    let get ({value; _} : 'a cell) : 'a = value
    let set (r : 'a cell) (x : 'a) : 'a cell = {r with value = x}

    let next ({next; _} : 'a cell) : 'a cell  = next
    let prev ({prev; _} : 'a cell) : 'a cell  = prev

    let of_list (lst : 'a list) : 'a t =
      let l = create () in
      match lst with
      | [] -> l
      | h :: t ->
         let first = push_front l h in
         let rec loop last = function
           | [] -> 
              last.next <- first;
              first.prev <- last;
              first
           | h :: t ->
              let nn = { value = h; next = first; prev = last } in
              last.next <- nn;
              loop nn t 
         in 
         let _ = loop first t in
         l
end
