(*Michail Denchev*)

let empty = function _ -> None;;
let insert d k v = function | x when x=k -> Some v | x -> d x;;
let find d k = d k;;

(*Joel Bjornson*)

module type Dict = sig
  type ('a, 'b) dict
  val empty : ('a, 'b) dict
  val insert : ('a, 'b) dict -> 'a -> 'b -> ('a, 'b) dict
  val find : ('a, 'b) dict -> 'a -> 'b option
end

module Fun_dict : Dict = struct
  type ('a, 'b) dict = 'a -> 'b option
  
  let empty _ = None
  
  let insert dict key vl = fun key' ->
    if key' = key then
      Some vl
    else
      dict key'

  let find dict = dict

end

(*Sen Han*)

module type Dictionary =
  sig
    type ('a, 'b) dict
    val empty : ('a, 'b) dict
    val insert : ('a, 'b) dict -> 'a -> 'b -> ('a, 'b) dict
    val find : ('a, 'b) dict -> 'a -> 'b option
    exception NotFound
  end

module FuncDictionary : Dictionary =
  struct
    type ('a, 'b) dict = 'a -> 'b
    exception NotFound

    let empty = fun _ -> raise NotFound
    let insert ( d : ('a, 'b) dict ) (k : 'a) (v : 'b) : ('a, 'b) dict =
      fun k2 ->
        if k2 = k then v else d k2
    let find (d : ('a, 'b) dict) ( k : 'a ) : 'b option =
      try
        Some (d k)
      with
      | NotFound -> None

  end
