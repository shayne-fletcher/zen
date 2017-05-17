type t =
| Lident of string
| Ldot of t * string
| Lapply of t * t

val flatten : t -> string list
val last : t -> string
val parse : string -> t

