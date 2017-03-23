(*Exercise 31 (Polynomials with one variable) from
  {{:https://caml.inria.fr/pub/docs/u3-ocaml/ocaml-modules.html#Exo-31}"Using,
  Understanding, and Unraveling the OCaml Language"} -- Didier Remy*)
#use "polynomial.ml";;

(*Polynomials with integer coefficients*)

module I = Polynomial (Ring_int_intr);;

(*Check equality of (1 + X) (1 - X) with (1 - X^2)*)
let p = I.mul (I.make [(1, 0); (1, 1)]) (I.make [(1, 0); (-1, 1)]);;
let q = I.make [(1, 0); (-1, 2)];;
I.equal p q;;

(*Polynomials with rational coefficients*)

module R = Polynomial (Ring_rat);;
let r = R.make ["1/2", 2];; (* (1/2) X^2 *)

(*Polynomials with float coefficients*)

module F = Polynomial (Ring_float);;
let r = F.make [string_of_float (sqrt 2.), 2];; (* (sqrt 2) X^2 *)

(*Polynomials with polynomial coefficients*)

module Y = Polynomial (I);;

(*Check equality of (X + Y) (X - Y) with (X^2 - Y^2)*)
(* (X + Y) *)
let r = Y.make [
  ([1, 1], 0); (*(1 X^1) Y^0*)
  ([1, 0], 1)  (*(1 X^0) Y^1*)
];;
(* (X - Y) *)
let s = Y.make [
  ([1, 1], 0); (*(1 X^1) Y^0*)
  ([-1, 0], 1) (*((-1) X^0) Y^1*)
];;
(* (X^2 - Y^2) *)
let t = Y.make [
  ([1, 2], 0);   (*(1 X^2) Y^0*)
  ([-1, 0], 2)  (* (-1 X^0) Y^2*)
];;
Y.equal (Y.mul r s) t;; (*True!*)

