#use "polynomial.ml";;

module R = Ring_int;;
module P = Polynomial (R);;

(*Check equality of (1 + x) (1 - X) with (1 - x^2)*)
let p = P.mul (P.make [(1, 0); (1, 1)]) (P.make [(1, 0); (-1, 1)]);;
let q = P.make [(1, 0); (-1, 2)];;
P.equal p q;;

