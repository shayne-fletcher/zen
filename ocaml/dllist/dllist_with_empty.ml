(**Doubly linked, circular list. This implementation admits the empty
   list.  *)
module type Doubly_linked_list_sig = 
  sig

    (**Will be abstract*)
    type 'a cell = {
      mutable value : 'a ;
      mutable next : 'a cell ;
      mutable prev : 'a cell ;
    }

    (**The type of a doubly-linked list*)
    type 'a t = 'a cell option ref

    (**Create a new (initially) empty list. This is an O(1)
       operation*)
    val create : unit -> 'a t

    (**Create a queue consisting of the singleton value [x]. This is
       an O(1) operation*)
    val singleton : 'a -> 'a t

    (**Converts from a normal list to a doubly-linked list. This is an
       O(N) operation*)
    val of_list : 'a list -> 'a t

    (**Create a new node and insert it into the list so that it is the
      first node. Returns the newly created cell. This is an O(1)
      operation*)
    val insert_first : 'a t -> 'a -> 'a cell

    (**Access the first cell of a list. This is an O(1) operation*)
    val first : 'a t -> 'a cell option

    (**[prepend n a] creates a new cell containing value [a] and
       inserts it into the list before cell [n]. Returns the new
       cell. This is an O(1) operation*)
    val prepend : 'a cell -> 'a -> 'a cell

    (**[append n a] creates a new cell containing value [a] and
       inserts it into the list after cell [n]. Returns the new
       cell. This is an O(1) operation*)
    val append : 'a cell -> 'a -> 'a cell

    (**Test if the given list is empty. This is an O(1) operation*)
    val is_empty : 'a t -> bool

    (**Compute the length of a list. This is an O(N) operation*)
    val length : 'a t -> int

    (**Given a cell, return the next cell in the list after the
       cell. The list is circular so when called on the last node of
       the list, the first node is returned as the next node. This is
       an O(1) operation*)
    val next : 'a cell -> 'a cell

    (**Given a cell, return the previous cell in the list before the
       cell. The list is circular so when called on the first node of
       the list, the last node is returned as the previous node. This
       is an O(1) operation*)
    val prev : 'a cell -> 'a cell

    (**Given a cell, get its value. This is an O(1) operation*)
    val get : 'a cell -> 'a

    (**Given a cell, set its value. This is an O(1) operation*)
    val set : 'a cell -> 'a -> 'a cell

    (**[map f l] creates a new list from l by applying f to each value
       of l in order. This is an O(N) operation*)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (**[copy l] returns a new list with elements that contain the same
        values as [l]. This is an O(N) operation*)
    val copy : 'a t -> 'a t

    (**Visit the cells of the list from first to last. This is an O(N)
      operation*)
    val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

    (**Visit the cells of the list from last to first. Since the list
      is bidirectional it doesn't suffer the performance problems of
      [List.fold_right]. This is an O(N) operation*)
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (**[find p l] finds the first cell in p that satisfies [p]
       @raise Not_found if the list is empty or no element satisfies
       [p]*)
    val find : ('a -> bool)  -> 'a t -> 'a cell

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

    let insert_first (l : 'a t) (x : 'a) : 'a cell =
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

    let prepend (node : 'a cell) (x : 'a) : 'a cell =
      let nn = { value = x; next = node; prev = node.prev; } in
      node.prev.next <- nn;
      node.prev <- nn;
      nn

    let append (node : 'a cell) (x : 'a) : 'a cell =
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
         let first = insert_first l h in
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

    let map (f : 'a -> 'b) (l : 'a t) : 'b t =
      match !l with
      | None -> ref None
      | Some node ->
         let rec first = {value = f node.value; next = first; prev = first} in
         let rec loop (last : 'b cell) (n : 'a cell) =
           if n == node then
             begin
               first.prev <- last;
               first
             end
           else
             begin
               let nn = {value = f n.value; next = first; prev = last} in
               last.next <- nn;
               loop nn n.next
             end in
         ref (Some (loop first node.next))

    let copy (l : 'a t) : 'a t = map (fun x -> x) l

    let fold_left (f : 'b -> 'a -> 'b) (init : 'b) (l : 'a t) : 'b =
      match !l with
      | None -> init
      | Some ({value; next; _} as node : 'a cell) ->
         let acc = f init value in
         let rec loop acc ({value; next; _} as n) =
           if n == node then
             acc
           else
             loop (f acc value) next in
         loop acc next

    let fold_right (f : 'a -> 'b -> 'b) (l : 'a t) (init : 'b) : 'b =
      match !l with
      | None -> init
      | Some ({value; prev; _} as node : 'a cell) ->
         let acc = init in
         let rec loop ({value; prev; _} as n : 'a cell) (acc : 'b) =
           if n == node then 
             f value acc
           else
             loop prev (f value acc) in
         loop prev acc

    let find (p : 'a -> bool) (l  : 'a t) : 'a cell = 
      match !l with
      | None -> raise Not_found
      | Some node ->
         let rec loop n =
           if n == node then raise Not_found
           else
             if p n.value then n else loop n.next
         in if p node.value then node else loop node.next

end
