val doctype : string (** [doctype] `<!DOCTYPE>` declaration. *)
val meta : string (** [meta] stock `<meta content=...>`. *)
val stylesheet : string (** [stylesheet] default style options. *)

val template : string -> string
(** [template src] produces a default html template for syntax
   highlighted blocks. *)

val all : string -> string
(** [all src] syntax highlights code blocks. *)

