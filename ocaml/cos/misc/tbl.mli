(**Association tables*)

(**The type of tables where ['a] is the key type and ['b] the type of
   values. Comparison of keys uses [Pervasives.compare]*)
type ('a, 'b) t

val empty : ('a, 'b) t
(**The empty table*)

val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
(**[add x v t] computes a new table from [t] in which [x] is bound to
   [v]*)

val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
(**[remove x t] computes a new table from [t] that does not contain a
   binding for [x]*)

val find : 'a -> ('a, 'b) t -> 'b
(**[find x t] searches for the ['b] associated with [x] in [t]. @raise
   Not_found if [x] is not in [t]*)

val mem : 'a -> ('a, 'b) t -> bool
(**[mem x t] searches for [t] for [x] returing [true] if found,
   [false] if not*)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(**[iter f tab] applies [f] to each binding in [t]*)

val map : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
(**[map f tab] computes a table with the same keys as [t] by
   application of [f] to each binding in [t]*)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(**[fold f t z] computes a value from a seed [z] by folding [f] over
   [t]*)

val print : (Format.formatter -> 'a -> unit) -> 
  (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a, 'b) t -> unit
(**[print print_key print_data ppf t] formats [t] on [ppf] by way of
   [print_key] and [print_data]*)
