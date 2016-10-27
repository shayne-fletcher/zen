module type SHOW = sig
  type t
  val show : t -> string
end

type 'a show_impl = (module SHOW with type t = 'a)

module Show_bool : SHOW with type t = bool = struct
  type t = bool
  let show = function | true -> "True" | false -> "False"
end

let show_bool = (module Show_bool : SHOW with type t = bool)

module Show_int : SHOW with type t = int = struct
  type t = int
  let show = Pervasives.string_of_int
end

let show_int = (module Show_int : SHOW with type t = int)

let print : 'a show_impl -> 'a -> unit =
  fun (type a) (show : a show_impl) (x : a) ->
  let module Show = (val show : SHOW with type t = a) in
  print_endline@@ Show.show x

let test_print_1 : unit = print show_bool true
let test_print_2 : unit = print show_int 3

module type NUM = sig
  type t
  val from_int : int -> t
  val ( + ) : t -> t -> t
end

type 'a num_impl = (module NUM with type t = 'a)

module Num_int : NUM with type t = int = struct
  type t = int

  let from_int x = x
  let ( + ) = Pervasives.( + )
end

let num_int = (module Num_int : NUM with type t = int)

module Num_bool : NUM with type t = bool = struct
  type t = bool

  let from_int = function | 0 -> false | _ -> true
  let ( + ) = function | true -> fun _ -> true | false -> fun x -> x
end

let num_bool = (module Num_bool : NUM with type t = bool)

let sum : 'a num_impl -> 'a list -> 'a = 
  fun (type a) (num : a num_impl) (ls : a list) ->
    let module Num = (val num : NUM with type t = a) in
    List.fold_right Num.( + ) ls (Num.from_int 0)

let test_sum = sum num_int [1; 2; 3; 4]

let print_incr : ('a show_impl * 'a num_impl) -> 'a -> unit =
  fun (type a) ((show : a show_impl), (num : a num_impl)) (x : a) ->
    let module Num = (val num : NUM with type t = a) in
    let open Num 
    in print show (x + from_int 1)

let print_incr_int (x : int) : unit = print_incr (show_int, num_int) x

(* -- *)

module type EQ = sig
  type t
  val eq : t * t -> bool
end

module Eq_bool : EQ with type t = bool = struct
  type t = bool
  let eq (a, b) = a = b
end

module Eq_int : EQ with type t = int = struct
  type t = int
  let eq (a, b) = a = b
end

module Eq_prod (X : EQ) (Y : EQ) : EQ with type t = X.t * Y.t = struct
  type t = X.t * Y.t
  let eq ((x1, y1), (x2, y2)) = X.eq (x1, x2) && Y.eq (y1, y2)
end

module type ORD = sig
  include EQ
  val lt : t * t -> bool
end

module Lt_int : ORD with type t = Eq_int.t = struct
  include Eq_int
  let lt (x, y) = Pervasives.( < ) x y
end

module Lt_prod (X : ORD) (Y : ORD) : ORD with type t = Eq_prod (X) (Y).t  = struct
  include Eq_prod (X) (Y)
  let lt ((x1, y1), (x2, y2)) = 
    X.lt (x1, x2) || X.eq (x1, x2) && Y.lt (y1, y2)
end
