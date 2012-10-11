type expr2 =
  | Var of string
  | IntConst of int
  | FloatConst of float
  | UnOp of (string*expr2)
  | BinOp of (string*expr2*expr2) 

let rec string_of_expr2 e =
  match e with 
  | Var s -> "Var ('"^s^"')"
  | IntConst i -> "IntConst ("^(string_of_int i)^")"
  | FloatConst f -> "FloatConst ("^(string_of_float f)^")"
  | UnOp (op, e) -> "UnOp ('"^op^"', "^(string_of_expr2 e)^")"
  | BinOp (op, e1, e2) -> "BinOp ('"^op^"', "^(string_of_expr2 e1)^", "^(string_of_expr2 e2)^")"
