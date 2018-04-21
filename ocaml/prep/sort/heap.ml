(* Heaps *)

module type S = sig
  val heap_sort : 'a array -> 'a array

  val heap_max : 'a array -> 'a
  val heap_extract_max : 'a array -> int -> 'a * int
  val heap_increase_key : 'a array -> int -> unit
  val max_heap_insert : 'a array -> int -> int -> int

end

module Heap = struct

  let parent i = i/2 - 1
  let left i   = 2 * i + 1
  let right i  = 2 * (i + 1)

  (* In a max-heap,[i (i <> 0). arr.(parent (i)) >= arr.(i)].
     That is, the value of a node is at most the value of its parent.
  *)

  let swap (arr : 'a array) (i : int) (j : int) =
    let t = arr. (j) in
    Array.set arr j (Array.get arr i);
    Array.set arr i t

  (* [max_heapfiy ~heapsize i] arranges for the subtree rooted at [i]
     to be a max-heap under the assumption the children of [i] are
     sub-heaps. *)
  let rec max_heapify (arr : 'a array) ~(heap_size : int) (i : int) =
    let l = left i in
    let r = right i in
    let largest =
      if l < 0 then i
      else if l >= heap_size then i
      else if arr. (l) > arr. (i) then l
      else i
    in
    let largest =
      if r < 0 then largest
      else if r >= heap_size then largest
      else if arr. (r) > arr. (largest) then r
      else largest
    in
    if largest <> i then (
      swap arr i largest;
      max_heapify arr ~heap_size largest
    )

  (* [build_max_heap arr] organizes [arr] into a max-heap. *)
  let build_max_heap (arr : 'a array) =
    let heap_size = Array.length arr in
    for i = (heap_size - 1) / 2 - 1 downto 0 do
      max_heapify arr ~heap_size i
    done

  (* [heap_sort arr] sorts in-place using heap-sort. *)
  let heap_sort (arr : 'a array) =
    let heap_size = ref (Array.length arr) in
    build_max_heap arr;
    for i = Array.length arr - 1 downto 1 do
      swap arr 0 i;
      heap_size := !heap_size - 1;
      max_heapify arr ~heap_size:!heap_size 0;
    done

  (* [heap_max arr] returns the element of [arr] with the largest
     key. *)
  let heap_max (arr : 'a array) = arr. (0)

  (* [heap_extract_max arr heap_size] removes and returns the element
     of [arr] with the largest key. It is to be understood that the
     returned heap is one element smaller. *)
  let heap_extract_max (arr : 'a array) (heap_size : int) =
    let () = if heap_size < 1 then failwith "heap underflow" else () in
    let max = arr. (0) in
    Array.set arr 0 (Array.get arr (heap_size - 1));
    max_heapify arr ~heap_size:(heap_size - 1) 0;
    max, (heap_size - 1)

  (* [heap_increase_key arr i key] increases the value of element
     [i]'s key to the new value [key] which is assumed to be at least
     as large as [i]'s current key value.

    - update arr.(i) to its new value - because the max-heap property
     may now not be satisfied

    - traverse a path from the node to the root

      - repeatedly compare key to to parent

      - exchange keys and continue of the elements's key is larger and
     terminate if the element's keys is smaller

 *)
  let heap_increase_key (arr : 'a array) (i : int) (key : int) =
    let () =
      if key < arr. (i) then
        failwith "new key is smaller than current key"
      else () in
    let i = ref i in
    Array.set arr !i key;
    while !i > 0 && arr. (parent !i) < arr. (!i ) do
      swap arr !i (parent !i);
      i := parent !i
    done

  (* [max_heap_insert arr heap_size key] inserts [key] into [arr]. It
     is understood the size of the resulting heap is one greater than
     the argument [heap_size]. *)
  let max_heap_insert (arr : 'a array) (heap_size : int) (key : int)  =
    let heap_size = heap_size + 1 in
    Array.set arr (heap_size - 1) min_int;
    heap_increase_key arr heap_size key;
    heap_size

end

let () =
  let arr = [| 5; 13; 2; 25; 7; 17; 20; 8; 4 |] in
  Heap.heap_sort arr;
  Array.iter (fun x -> Printf.printf "%d, " x) arr;
  Printf.printf "\n";
  flush stdout

