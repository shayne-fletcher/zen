(**Parser errors*)

(**The type of syntax errors*)
type error =
(**Not one of the following*)
| Other of Ml_location.t 
(**A construction is not closed*)
| Unclosed of Ml_location.t * string * Ml_location.t * string 
(**An unexpected term*)
| Not_expecting of Ml_location.t * string

(**The type of syntax error exceptions*)
exception Error of error
