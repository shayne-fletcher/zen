(**Access paths*)

(**The type of paths*)
type t =
  | Pident of Ident.t (**An identifier*)
  | Pdot of t * string * int (**Access to a module component*)
  | Papply of t * t  (**Expressions like [module N = F (M)] (one assumes)*)

val same : t -> t -> bool
val is_free : Ident.t -> t -> bool
val binding_time : t -> int
val name : ?paren:(string -> bool) -> t -> string
val head : t -> Ident.t
val heads : t -> Ident.t list
val last : t -> string

type typath =
  | Regular of t
  | Ext of t * string
  | Local_ext of Ident.t
  | Cstr of t * string

val constructor_typath : t -> typath
val is_constructor_typath : t -> bool
