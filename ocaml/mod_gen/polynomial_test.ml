#use "polynomial.ml";;

(*Polynomials with integer coefficients*)

module R = Ring_int;;
module P = Polynomial (R);;

(*Check equality of (1 + X) (1 - X) with (1 - X^2)*)
let p = P.mul (P.make [(1, 0); (1, 1)]) (P.make [(1, 0); (-1, 1)]);;
let q = P.make [(1, 0); (-1, 2)];;
P.equal p q;;

(*Polynomials with polynomial coefficients*)

module Y = Polynomial (P);;

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
