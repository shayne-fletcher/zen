type ('a, 'b) term = 
| Term of 'a * ('a, 'b) term list
| Var of 'b
;;
