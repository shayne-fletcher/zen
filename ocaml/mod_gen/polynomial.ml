(*Exercise 31 (Polynomials with one variable) from
  {{:https://caml.inria.fr/pub/docs/u3-ocaml/ocaml-modules.html#Exo-31}"Using,
  Understanding, and Unraveling the OCaml Language"} -- Didier Remy*)

#load "nums.cma";;

(*Type of a module implementing arithmetic*)
module type ARITH = sig
  type t
  val of_int : int -> t              val to_int : t -> int
  val of_string : string -> t        val to_string : t -> string
  val zero : t                       val one : t
  val add : t -> t -> t              val sub : t -> t -> t
  val mul : t -> t -> t              val div : t -> t -> t
  val compare : t -> t -> int        val equal : t -> t -> bool
end;;

(*Arithmetic with [int]*)
module Int : ARITH = struct
    type t = int
    let of_int x = x                   let to_int x = x
    let of_string = int_of_string      let to_string = string_of_int
    let zero = 0                       let one = 1
    let add = ( + )                    let sub = ( - )
    let mul = ( * )                    let div = ( / )
    let compare = Pervasives.compare   let equal = ( = )
end;;

(*Arithmetic with [float]*)
module Float : ARITH = struct
  type t = float
  let of_int = float_of_int          let to_int = int_of_float
  let of_string = float_of_string    let to_string = string_of_float
  let zero = 0.                       let one = 1.
  let add = ( +. )                    let sub = ( -. )
  let mul = ( *. )                    let div = ( /. )
  let compare = Pervasives.compare   let equal = ( = )
end;;

(*Arithmetic with [Num.num] (rational numbers)*)
module Rat : ARITH = struct
  include Num
  type t = num
  let of_int = num_of_int            let to_int = int_of_num
  let of_string = num_of_string      let to_string = string_of_num
  let compare = compare_num          let equal x y = compare x y = 0
  let zero = Int 0                   let one = Int 1
  let add = add_num                  let sub = sub_num
  let mul = mult_num                 let div = div_num
end;;

(*The type of an additive group*)
module type ADDITIVE_GROUP = sig
  type t                              type extern_t
  val make : extern_t -> t            val show : t -> extern_t
  val print : t -> unit               val zero : t
  val add : t -> t -> t               val inverse : t -> t
  val equal : t -> t -> bool
end;;

(*The type of a set equipped with two binary operations that
  generalize the arithmetic operations of addition and
  multiplication*)
module type RING = sig
  include ADDITIVE_GROUP
  val one : t                          val mul : t -> t -> t
end;;

(*Build a ring over an arithmetic using [int] for the external
  representation type [extern_t]*)
module Ring_int (A : ARITH) :
  RING  with type t = A.t and type extern_t = int =
struct
  include A
  type extern_t = int

  let make = of_int
  let show = to_int
  let inverse = sub zero
  let print x = print_int (show x)
end;;

(*Build a ring over an arithmetic using [string] for the external
  representation type [extern_t]*)
module Ring_str (A : ARITH) :
  RING  with type t = A.t and type extern_t = string =
struct
  include A
  type extern_t = string

  let make = of_string
  let show = to_string
  let inverse = sub zero
  let print x = print_string (show x)
end;;

(*Rings over various arithmetics*)
module Ring_int32 = Ring_int (Int32);;
module Ring_int64 = Ring_int (Int64);;
module Ring_nativeint = Ring_int (Nativeint);;
module Ring_int_intr = Ring_int (Int);;
module Ring_rat = Ring_str (Rat);;
module Ring_float = Ring_str (Float);;

(*The type of polynomials*)
module type POLYNOMIAL = sig
  type coeff (*Type of coefficients*)
  type coeff_extern_t (*Type of coeff. external rep*)

  (*Polynomials with coefficients drawn from a ring are themselves are
    rings*)
  include RING (*Declares a type [t] and [extern_t]*)

  (*Function to evaluate a polynomial at a point*)
  val eval : t -> coeff -> coeff
end;;

(*Functor for building polynomials from rings*)
module Polynomial (R : RING) :
  POLYNOMIAL with type coeff = R.t
  and type coeff_extern_t = R.extern_t
  and type extern_t = (R.extern_t * int) list =
struct

  type coeff = R.t (*Coefficient type*)
  type coeff_extern_t = R.extern_t (*External coeff. rep*)
  type extern_t = (coeff_extern_t * int) list (*External polynomial rep*)

  (*List of coefficients and their powers*)
  type t = (coeff * int) list (*Invariant : Ordered by powers,
                                lower order terms at the front*)

  (*Simple printing*)
  let print p =
    List.iter
      (fun (c, k) -> Printf.printf "+ (";
        R.print c;
        Printf.printf ")X^%d " k)
      p

  (*Null coefficients are eliminated in order to get a canonical
    representation. In particular, the null monomial is the empty
    list*)
  let zero = []

  (*This is superflous in that it is a particular monomial however,
    it's presence makes polynomials satisfy the interface of rings *)
  let one = [R.one, 0]

  (*[monomial c k] computes the monomial {%c^{k}%} for non-negative
    [k]*)
  let monomial (a : coeff) (k : int) =
    if k < 0 then
      failwith "monomial : negative powers not supported"
    else if R.equal a R.zero then [] else [a, k]

  (*Addition; Much care taken here to maintain the representation
    invariant*)
  let rec add u v =
    match u, v with
    | [], _ -> v
    | _, [] -> u
    | ((c1, k1) :: r1 as p1), ((c2, k2) :: r2 as p2) ->
      if k1 < k2 then
        (c1, k1) :: (add r1 p2)
      else if k1 = k2 then
        let c = R.add c1 c2 in
        if R.equal c R.zero then add r1 r2
        else (c, k1) :: (add r1 r2)
      else (c2, k2) :: (add p1 r2)

  (*[make] follows from [addd]*)
  let make l =
    List.fold_left (fun acc (c, k) ->
      add (monomial (R.make c) k) acc) zero l

  (*[show p] produces an external representation of [p]*)
  let show p =
    List.fold_right (fun (c, k) acc -> (R.show c, k) :: acc) p []

  (*[inverse p] is the additive inverse of [p]*)
  let inverse (p : t ) : t = 
    List.rev (
      List.fold_left (
        fun acc (a, k) -> (R.inverse a, k) :: acc) zero p)

  (*Premultiply a polynomial by a monomial*)
  let rec times (c, k) = function
    | [] -> []
    | (c1, k1) :: q ->
      let c2 = R.mul c c1 in
      if R.equal c2 R.zero then times (c, k) q
      else (c2, k + k1) :: times (c, k) q

  (*Multiplication*)
  let mul p = List.fold_left (fun r m -> add r (times m p)) zero

  (*Equality*)
  let rec equal p1 p2 =
    match p1, p2 with
    | [], [] -> true
    | (c1, k1) :: q1, (c2, k2) :: q2 ->
      k1 = k2 && R.equal c1 c2 && equal q1 q2
    | _ -> false

  (*Compute {%c^{k}%} using the method of
    {{:https://en.wikipedia.org/wiki/Exponentiation_by_squaring}Exponentiation
    by Squaring}*)
  let rec pow c = function
    | 0 -> R.one
    | 1 -> c
    | k ->
      let l = pow c (k lsr 1) in
      let l2 = R.mul l l in
      if k land 1 = 0 then l2 else R.mul c l2

  (*[eval p c] evaluates {%p(c)%} using
    {{:https://en.wikipedia.org/wiki/Horner's_method}Horner's rule (for
    computational efficient evaluation)}*)
  let eval p c = match List.rev p with
    | [] -> R.zero
    | (h :: t) ->
      let reduce (a, k) (b, l) =
        let n = pow c (k - l) in
        let t = R.add (R.mul a n) b in
        (t, l)  in
      let a, k = List.fold_left reduce h t in
      R.mul (pow c k) a
end;;
