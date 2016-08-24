(**The type of an ident*)
type t = {
  stamp : int;  (**Binding time*)
  name : string; (**The identifier name*)
  mutable flags : int (**Global, predef. exn, etc.*)
}

val create : string -> t
(**Creates an identifier from a name and a unique binding time*)

val create_persistent : string -> t
(**Like [create] but sets stamp but with a binding time of zero and a
   flag set indicating identifier is a global*)

val create_predef_exn : string -> t
(**Like [create] but with a flag set indicating the identifier is a
   predef exn.*)

val rename : t -> t
(**A new identifier sharing the same name but a new unique binding
   time*)

val name : t -> string
(**Projection of the name field*)

val binding_time : t -> int
(**Projection of the stamp field*)

val unique_name : t -> string
(**The identifier name concatenated with its stamp*)

val persistent : t -> bool
(**A stamp of zero indicates a persistent identifier*)

val equal : t -> t -> bool
(**Compare identifiers by name*)

val same : t -> t -> bool
(**Compare identifiers by binding location. Two identifiers are the
   same if either they agree in all fields*)

val hide : t -> t
(**Compute an identifier with the same name as the given identifier
   but stamp different from any stamp returned by [create]. When put in
   an ['a tbl], this identifier can only be looked up by name*)

val compare : t -> t -> int
(**Lexicographically order identifiers by binding location*)

val make_global : t -> unit
(**Change an identifier to be global*)

val global : t -> bool
(**Determine if an identifier is global*)

val is_predef_exn : t -> bool
(**Determine if an identifier is a predef. exn.*)

val current_time : unit -> int
(**Retrieve the current binding time*)

val set_current_time : int -> unit
(**Change the current binding time*)

val reinit : unit -> unit
(**Basically resets the current binding time to its start value*)

val print : Format.formatter -> t -> unit

(**Association tables from identifiers to type ['a]. Only the [name]
   field of the identifiers participates in the table ordering*)
type 'a tbl

val empty : 'a tbl
(**The empty table*)

val add : t -> 'a -> 'a tbl -> 'a tbl
(**[add id data tbl] associates [id] with [data] in [tbl] storing any
   previous association if there is one*)

val find_same : t -> 'a tbl -> 'a
(**[find_same t tbl] looks for a [data] in the table associated with
   exactly [t] (considers timestamps)*)

val find_name : string -> 'a tbl -> 'a
(**[find_name s tbl] finds data associated with [s] without
   consideration of timestamps*)

val find_all : string -> 'a tbl -> 'a list
(**Find all of the ['a] values in the table associated with the given
   [name]*)

val fold_name : (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b
(**Fold over the identifiers and their current current associated
   values in the table. Does not consider previous values*)

val fold_all : (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b
(**Fold over each identifier and all associated data in the table*)

val iter : (t -> 'a -> unit) -> 'a tbl -> unit
(**Iterate over each node in the table*)
