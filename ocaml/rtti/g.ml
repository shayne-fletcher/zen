type _ value =
  | V_int : int -> int value
  | V_bool : bool -> bool value

type _ expr =
  | E_value : 'a value -> 'a expr
  | E_if : bool expr * 'a expr * 'a expr -> 'a expr
  | E_eq : 'a expr * 'a expr -> bool expr
  | E_lt : int expr * int expr -> bool expr

let rec eval : type a. a expr -> a = function
  | E_value (V_int i) -> i
  | E_value (V_bool b) -> b  
  | E_if (b, l, r) -> if eval b then eval l else eval r
  | E_eq (l, r) -> eval l = eval r
  | E_lt (l, r) -> eval l < eval r

let _ = 
  eval (
     E_if (
       E_lt (E_value (V_int 2), E_value (V_int 4)),
       E_value (V_int 42),
       E_value (V_int 0))
  )
