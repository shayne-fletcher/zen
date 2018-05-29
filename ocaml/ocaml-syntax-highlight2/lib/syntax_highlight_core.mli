val template : string -> string
(** [template src] produces a html template with a default style-sheet
   for syntax highlighted blocks. *)

val all : string -> string
(** [all src] translates [src] to a new syntax highlighted version. *)
