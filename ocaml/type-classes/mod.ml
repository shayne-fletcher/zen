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

let test_int = assert (Eq_int.eq (1, 1) && not (Eq_int.eq (2, 1)))

module type EQ_PROD =
  functor (X : EQ) (Y : EQ) -> EQ with type t = X.t * Y.t

module Eq_prod : EQ_PROD =
  functor (X : EQ) (Y : EQ) -> struct
    type t = X.t * Y.t

    let eq ((x1, y1), (x2, y2)) =  X.eq (x1, x2) && Y.eq(y1, y2)
end

module Eq_bool_int : 
  EQ with type t = (bool * int) = Eq_prod (Eq_bool) (Eq_int)

module type ORD = sig
  include EQ

  val lt : t * t -> bool
end

module Ord_int : ORD with type t = int = struct
  include Eq_int

  let lt (x, y) = Pervasives.( < ) x y
end

module type ORD_PROD =
  functor (X : ORD) (Y : ORD) -> ORD with type t = X.t * Y.t

module Ord_prod : ORD_PROD =
  functor (X : ORD) (Y : ORD) -> struct
    include Eq_prod (X) (Y)

    let lt ((x1, y1), (x2, y2)) =
      X.lt (x1, x2) || X.eq (x1, x2) && Y.lt (y1, y2)
  end

module Ord_int_int = Ord_prod (Ord_int) (Ord_int)

let test_ord_int_int = 
  let x = (1, 2) and y = (1, 4) in
  assert ( not (Ord_int_int.eq (x, y)) && Ord_int_int.lt (x, y))

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

let show_int : int show_impl = (module Show_int : SHOW with type t = int)

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

module type LIST_SHOW =
  functor (X : SHOW) -> SHOW with type t = X.t list

module List_show : LIST_SHOW =
  functor (X : SHOW) -> struct
    type t = X.t list

    let show =
        fun xs ->
          let rec go first = function
            | [] -> "]"
            | h :: t ->
              (if (first) then "" else ", ") ^ X.show h ^ go false t in
          "[" ^ go true xs
  end

let show_list : 'a show_impl -> 'a list show_impl =
  fun (type a) (show : a show_impl) ->
    let module Show = (val show : SHOW with type t = a) in
    (module struct
      type t = a list

      let show : t -> string =
        fun xs ->
          let rec go first = function
            | [] -> "]"
            | h :: t ->
              (if (first) then "" else ", ") ^ Show.show h ^ go false t in
          "[" ^ go true xs
    end : SHOW with type t = a list)

let testls : string = let module Show =
     (val (show_list show_int) : SHOW with type t = int list) in
  Show.show (1 :: 2 :: 3 :: [])

module type MUL = sig
  include EQ
  include NUM with type t := t

  val mul : t -> t -> t
end

type 'a mul_impl = (module MUL with type t = 'a)

(*The type of a functor taking [EQ] and [NUM] base class arguments
  that "returns" a [MUL] class instance*)
module type MUL_F =
  functor (E : EQ) (N : NUM with type t = E.t) -> MUL with type t = E.t

(*Functor implementation for generating a "default" [MUL] instance
  given instances of a (compatible) [EQ] and [NUM]*)
module Mul_default : MUL_F =
  functor (E : EQ) (N : NUM with type t = E.t)  -> struct
    include (E : EQ with type t = E.t)
    include (N : NUM with type t := E.t)

    let mul : t -> t -> t =
      let rec loop x y = begin match () with
        | () when eq (x, (from_int 0)) -> from_int 0
        | () when eq (x, (from_int 1)) -> y
        | () -> y + loop (x + (from_int (-1))) y
      end in loop

end

module Mul_bool : MUL with type t = bool = Mul_default (Eq_bool) (Num_bool)

module Mul_int : MUL with type t = int = struct
  include (Eq_int : EQ with type t = int)
  include (Num_int : NUM with type t := int)

  let mul = Pervasives.( * )
end

let dot : 'a mul_impl -> 'a list -> 'a list -> 'a =
  fun (type a) (mul : a mul_impl) ->
    fun xs ys ->
      let module M = (val mul : MUL with type t = a) in
      sum (module M : NUM with type t = a)@@ List.map2 M.mul xs ys

let test_dot =
  dot (module Mul_int : MUL with type t = int) [1; 2; 3] [4; 5; 6]

let rec replicate : int -> 'a -> 'a list =
  fun n x -> if n <= 0 then [] else x :: replicate (n - 1) x

let rec print_nested : 'a. 'a show_impl -> int -> 'a -> unit =
  fun show_mod -> function
  | 0 -> fun x -> print show_mod x
  | n -> fun x -> print_nested (show_list show_mod) (n - 1) (replicate n x)

(*
let test_nested =
  let n = read_int () in
  print_nested (module Show_int : SHOW with type t = int) n 5
*)
