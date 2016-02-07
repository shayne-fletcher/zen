module type Persistent_array_sig = sig
  type 'a t (*the array type*)

  val init : int -> (int -> 'a) -> 'a t
  val make : int -> 'a -> 'a t 
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val string_of_array : ('a -> string) -> 'a t -> string
  
end

module Persistent_array : Persistent_array_sig = struct

  type 'a t = 'a data ref
  and 'a data = Arr of 'a array | Diff of int * 'a * 'a t

  let init (n : int) (f : int -> 'a) : 'a t = 
    ref (Arr (Array.init n f))

  let make (n : int) (x : 'a) = 
    ref (Arr (Array.make n x))

  let rec reroot (t : 'a t) : unit =
    match !t with
    | Arr _ -> ()
    | Diff (i, v, t') ->
      reroot t';
      begin 
        match !t' with
        | Arr a as n ->
          let v' = a.(i) in
          a.(i) <- v;
          t := n;
          t' := Diff (i, v', t)
        | Diff _ -> assert false
      end

  let rec get (t : 'a t) (i : int) : 'a = 
    match !t with
    | Arr a -> a.(i)
    | Diff _ ->
      reroot t;
      begin match !t with
      | Arr a -> a.(i)
      | Diff _ -> assert false
      end

  let rec set (t : 'a t) (i : int) (v : 'a) : 'a t =
    reroot t;
    match !t with
    | Arr a as n ->
      let old = a.(i) in
      a.(i) <- v;
      let res = ref n in
      t := Diff (i, old, res);
      res
    | Diff _ -> assert false

  let rec string_of_array (f : 'a -> string) (t : 'a t) : string =
    match !t with
    | Arr a -> 
      "[|" ^ 
        (String.concat "; " (List.map f (Array.to_list a))) 
      ^ "|]"
    | Diff (i, v, t') -> 
      "Diff (" 
      ^ (string_of_int i) 
      ^ ", " 
      ^ (f v) 
      ^ ", " 
      ^ (string_of_array f t') ^ ")"

end

open Persistent_array ;;

let a0 = init 7 (fun _ -> 0) ;;
let a1 = set a0 1 7 ;;
let a2 = set a1 2 8 ;;
let a3 = set a1 2 9 ;;
string_of_array string_of_int a0 ;;
string_of_array string_of_int a1 ;;
string_of_array string_of_int a2 ;;
string_of_array string_of_int a3;;
