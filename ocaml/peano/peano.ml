module Dim :
 sig

   type +'a suc
   type (+'a, +'b, +'c, +'d) t

   val ( + ) : ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
   val ( - ) : ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
   val ( * ) : ('a0, 'b0, 'c0, 'd0) t -> ('b0, 'b1, 'd0, 'd1) t -> ('a0, 'b1, 'c0, 'd1) t
   val ( / ) : ('a0, 'b0, 'c0, 'd0) t -> ('a1, 'b0, 'c1, 'd0) t -> ('a0, 'a1, 'c0, 'c1) t

   val meters : float -> ('a, 'a s, 'b, 'b) t
   val seconds : float -> ('a, 'a, 'b, 'b s) t

   val as_float : ('a, 'b, 'c, 'd) t -> float

 end = struct

   type +'a suc = 'a * 'a
   type (+'a, +'b, +'c, +'d) t = float

   let ( + ) = ( +. )
   let ( - ) = ( -. )
   let ( * ) = ( *. )
   let ( / ) = ( /. )

   let meters x = x
   let seconds x = x
   let as_float x = x

end;;

(*
type z  (*zero*)
type +'n s (*successor*)

module Simple_n_list : sig

  type ('n, 'a) t
  val nil : (z, 'a) t
  val cons : 'a * ('n, 'a)t -> ('n s, 'a) t
    
end = struct
  type ('n, 'a) t = 'a list
  
  let nil = []
  let cons (x, xs) = x :: xs

end

module Nat : sig
  type +'i t

  val zero : ('m * 'm) t
  val succ : ('m * 'n) t -> (('m * 'n s)) t
  val add : ('m * 'n) t * ('l * 'm) t -> ('l * 'n) t
  val to_int : 'i t -> int

end = struct

  type 'i t = int
  let zero = 0
  let succ n = n + 1
  let add (n, m) = n + m
  let to_int n = n

end
*)
