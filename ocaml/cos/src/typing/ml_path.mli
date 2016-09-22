(**Access paths*)

(**The type of paths*)
type t =
  | Pident of Ml_ident.t (**An identifier*)
  | Pdot of t * string * int (**Access to a module component*)
  | Papply of t * t  (**Expressions like [module N = F (M)] (one assumes)*)

val same : t -> t -> bool
val is_free : Ml_ident.t -> t -> bool
val binding_time : t -> int
val name : ?paren:(string -> bool) -> t -> string
val head : t -> Ml_ident.t
val heads : t -> Ml_ident.t list
val last : t -> string

type typath =
  | Regular of t
  | Ext of t * string
  | Local_ext of Ml_ident.t
  | Cstr of t * string

val constructor_typath : t -> typath
val is_constructor_typath : t -> bool
