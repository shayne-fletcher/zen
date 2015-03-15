(*Doubly linked list*)

type 'a node = { 
  mutable value : 'a;
  mutable next : 'a node;
  mutable prev : 'a node
}

exception Empty

type 'a t = 'a node

(*The [next] and [prev] fields in the single node list, point to the
  node itself*)
let singleton (x : 'a) : 'a node = 
  let rec nn = { value = x; next = nn ; prev = nn } in nn

let append (l : 'a node) (x : 'a) : 'a node =
  let nn = { value = x; next = l.next; prev = l } in
  l.next.prev <- nn;
  l.next <- nn;
  nn

let prepend (l : 'a node) (x : 'a) : 'a node =
  let nn = { value = x; next = l; prev = l.prev; } in
  l.prev.next <- nn;
  l.prev <- nn;
  nn

let of_list lst =
  match lst with
  | [] -> raise Empty
  | h :: t ->
     let first = singleton h in
     let rec loop last = function
       | [] ->
          last.next <- first;
          first.prev <- last;
          first
       | h :: t ->
          let nn = { value = h; next = first; prev = last } in
          last.next <- nn;
          loop nn t 
     in loop first t
