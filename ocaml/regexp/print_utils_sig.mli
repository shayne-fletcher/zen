module type S = sig

  (**A function to produce a string representation of a list. The
     first argument, [f] is a function that given an element of the
     list, produces a string representation of that element. The
     syntax chosen is [x; y; z; ...] where x, y and z represent
     elements of the list*)
  val string_of_list : ('a -> string) -> 'a list -> string

  (**This function produces a string representation of an array. The
     first argument [f], is a function that given an element of the
     list, produces a string representation of that element. The
     syntax chosen is [|x; y; z; ...|] where x, y and z represent
     elements of the list*)
  val string_of_array : ('a -> string) -> 'a array -> string

  (**Produce a string representation of a [Set]. In [string_of set f
     fold s], [f] is a function that given an element of the provided
     set [s], produces a string representation of that element, [fold]
     is the function to fold over the set produced by the [Set.Make]
     functor e.g. [Int_set.fold]. The syntax chosen is [ x, y, z, ...]
     where x, y and z represent elements of the set*)
  val string_of_set : ('a -> 'b) -> (('a -> 'b list -> 'b list) -> 'c -> 'd list -> string list) -> 'c -> string

end
