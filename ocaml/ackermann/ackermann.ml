let err f = Format.sprintf f

let rec ackermann : int * int -> int = function
  | (0, n) -> n + 1
  | (m, 0) when m > 0 -> ackermann (m - 1, 1)
  | (m, n) when m > 0 && n > 0 -> ackermann (m - 1, ackermann (m, n - 1))
  | (m, n) -> invalid_arg (err "ackermann : m=%d, n=%d" m n);;


