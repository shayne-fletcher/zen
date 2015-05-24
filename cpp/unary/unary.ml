type num = Z | S of num

let scc (x : num) : num = S x

let prd : num -> num = function | S n -> n | _ -> Z

let rec add (x : num) (y : num) : num =
  match (x, y) with 
  | (Z, _) -> y
  | (_, Z) -> x
  | (S m, n) -> scc (add m n)

let rec sub (x : num) (y : num) : num =
  match (x, y) with
  | (Z, _) -> Z
  | (n, Z) -> n
  | (S m, n) -> sub m (prd n)

let rec mul (x : num) (y : num) : num = 
  match (x, y) with
  | (Z, _) -> Z
  | (_, Z) -> Z
  | (S Z, x) -> x
  | (x, S Z) -> x
  | (S m, n) -> add (mul m n) n
