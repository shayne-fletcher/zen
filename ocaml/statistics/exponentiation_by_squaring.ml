(** Exponentiation by squaring.

    Fast method for computation of large positive integer powers of a
    number.

    The method is based on the observation that, for a positive
    integer n we have

        x^n = 
          | n even -> (x^2)^(n / 2)
          | n odd -> x * (x^2)^((n - 1) / 2).

*)

let rec exp_by_squaring x n =
  if n < 0 then exp_by_squaring (1 / x) (-n)
  else if n = 0 then 1
  else if n = 1 then x
  else if n mod 2 = 0 then exp_by_squaring (x*x) (n/2)
  else (*n is odd*) x * exp_by_squaring (x*x) ((n - 1) / 2)

let _ = Printf.printf "%d\n" (exp_by_squaring 3 2)
