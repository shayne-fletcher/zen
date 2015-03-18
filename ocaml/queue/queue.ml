(*First-in first-out queues with in-place modification. The queue is
  organized as a singly-linked cyclic list. 
*)
module type Queue_sig = sig

  type 'a t

  exception Empty

  val create : unit -> 'a t
  val add : 'a t -> 'a -> unit
  val take : 'a t -> 'a
  val length : 'a t -> int
  val peek : 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val copy : 'a t -> 'a t
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val clear : 'a t -> unit
  val is_empty : 'a t -> bool
  val length : 'a t -> int

end

module Queue : Queue_sig = struct

  type 'a cell = {
    value : 'a ;
    mutable next : 'a cell ;
  }

  type 'a t = {
    mutable length : int ;
    mutable tail : 'a cell option ;
  }

  exception Empty

  let deref : 'a option -> 'a = 
    function | Some x -> x | _ -> failwith "None"
                                           
  let create () : 'a t = { length = 0; tail = None }

  let clear (q : 'a t) : unit =
    q.length <- 0;
    q.tail <- None

  let is_empty (q : 'a t) : bool = q.length = 0

  let length (q : 'a t) : int = q.length

  let add (q : 'a t) (x : 'a) : unit =
    if q.length = 0 then
      let rec cell = {value = x; next = cell} in
      q.length <- 1 ;
      q.tail <- Some cell
    else
      let tail = deref q.tail in
      let head = tail.next in
      let cell = {value = x; next = head} in
      q.length <- q.length + 1 ;
      tail.next <- cell ;
      q.tail <- Some cell

  let take (q : 'a t) : 'a =
    if q.length = 0 then raise Empty;
    q.length <- q.length - 1;
    let tail = deref q.tail in
    let head = tail.next in
    if head == tail then
      q.tail <- None
    else
      tail.next <- head.next;
    head.value

  let length (q : 'a t) : int = q.length
                                  
  let peek (q : 'a t) : 'a =
    if q.length = 0 then raise Empty ;
    let tail = deref q.tail in
    let head = tail.next in
    head.value

  let map (f : 'a -> 'b) (q : 'a t) =
    if length q = 0 then create ()
    else
      begin
        let tail = deref q.tail in
        let rec tail' = {value = f tail.value; next = tail' } in

        let rec loop (prev : 'b cell) (cell : 'a cell) =
          if cell != tail then 
            let res = {value = f cell.value; next = tail' } 
            in  prev.next <- res; in

        loop tail' tail.next;
        { length = q.length; tail = Some tail' }
      end

  let copy (q : 'a t) : 'a t = map (fun x -> x) q

  let fold (f : 'b -> 'a -> 'b) (init : 'b) (q : 'a t) : 'b =
    if length q == 0 then 
      init
    else
      let tail = deref q.tail in
      let head = tail.next in
      let rec loop acc cell =
        if cell == head then 
          acc
        else
          loop (f acc cell.value) cell.next in
      loop  (f init head.value) head.next

end
