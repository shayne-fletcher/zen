type t = 
| Free of string
| Bound of int
| Abs of string * t
| Apply of t * t
