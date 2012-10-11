type expr2 =
  | Var of string
  | Const of int
  | UnOp of (string*expr2)
  | BinOp of (string*expr2*expr2) 

let rec string_of_expr2 e =
  match e with 
  | Var s -> "Var ('"^s^"')"
  | Const i -> "Const ("^(string_of_int i)^")"
  | UnOp (op, e) -> "UnOp ('"^op^"', "^(string_of_expr2 e)^")"
  | BinOp (op, e1, e2) -> "BinOp ('"^op^"', "^(string_of_expr2 e1)^", "^(string_of_expr2 e2)^")"
