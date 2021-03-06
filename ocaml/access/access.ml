let x = [`On; `Off];;
(*
val x : [> `Off | `On ] list = [`On; `Off]

- x is a list of values of a type that at least contain the
  constructors `Off and `On
*)

let n = `Number 1;;
(*
val n : [> `Number of int ] = `Number 1

- n is a type that at least has the constructor `Number of int
*)

let f = function | `On -> 1 | `Off -> 0 | `Number n -> n;;
(*
val f : [< `Number of int | `Off | `On ] -> int = <fun>

- The argument to f has a type that at most has the constructors `Off,
  `On and `Number
*)

let f x = (x :> [`A | `B]);;
(*val f : [< `A | `B ] -> [ `A | `B ] = <fun>

  - We see `x` needs to be coercible to type type [`A | `B]
  - So, we read [< `A | `B ] as a type that at most contains the tags
    `A and `B
  - [ `A ], [ `B ] and [ `A | `B ], are the sub-types of [ `A | `B ]
  - [< `A | `B] is the set of sub-types of [`A | `B]
*)


let f x = (x :> [`A | `B] * [`C | `D]);;
(*
val f : [< `A | `B ] * [< `C | `D ] -> [ `A | `B ] * [ `C | `D ] = <fun>

  - We see `x` needs to be corecible to [ `A | `B ] * [ `C | D` ]
  - This coercion is valid if
    - x is a pair where
      - The first component is a sub-type of [`A | `B]
      - The second component is a sub-type of [`C | `D]
  - [< `A | `B] * [< `C | `D] is the set of sub-types of [ `A | `B ] *
    [ `C | `D ]
*)

let f x = (x  :> [`A] -> [`C | `D]);;
(*val f : ([> `A ] -> [< `C | `D ]) -> [ `A ] -> [ `C | `D ] = <fun>

  - We see `x` needs to be coercible to [`A] -> [`C | `D]
  - This coercion is valid if
    - x is an arrow where
      - The argument is of a type that at least contains the tag `A,
        that is [ `A ] and [ `A | ... ]
  - [> `A] is the set of of super-types of [`A]
  - The return value is a sub-type of [`C | `D]
  - [> `A ] -> [< `C | `D ] is the set of sub-types of [ `A ] -> [
  `C | `D ]

(* -- *)

(*https://blogs.janestreet.com/a-and-a/*)

module type S = sig
  type (+'a, +'b) s
  type (-'a, +'b) t
end;;

module M : S = struct
  type ('a, 'b) s = 'a * 'b
  type ('a, 'b) t = 'a -> 'b
end;;

let f x = (x : ([`A], [`B]) M.s :> ([`A | `C], [`B | `D]) M.s);;
let f x = (x : ([`A | `B], [`C]) M.t :> ([`A], [`C | `D]) M.t);;

(* -- *)

(*http://alan.petitepomme.net/cwn/2012.12.04.html*)

(*The presence of the constraint disables variance inference*)
type +'a t = {x : 'a} constraint 'a = [< `A | `B];;
let f x = (x : [`A] t :> [`A | `B] t);;


type 'a t = {x : int};;
let f x = (x : [`A] t :> [`B] t);;
*)

module type S = sig
  type 'a t constraint 'a = [< `A | `B ]
  val init : [`A] t
  val f : [`A] t -> [`B] t
end;;

module T : S = struct
  type 'a t = {x : int} constraint 'a = [< `A | `B]
  let init = {x = 0}
  let f x = x
end;;

module M : S = struct
  type 'a t = unit constraint 'a  = [< `A | `B ]
  let init = ()
  let f x = x
end;;

