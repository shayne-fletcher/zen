(*Requires compiling with '-rectypes'

  "Allow arbitrary recursive types during type-checking. By default,
  only recursive types where the recursion goes through an object type
  are supported. Note that once you have created an interface using this
  flag, you must use it again for all dependencies."

  Admits the typing of expressions like:

    type t = unit -> t
    let rec f : (unit -> int * 'a) as 'a = fun () -> (1, f)

  When -rectypes is not specified, constructors are required as in:

    type t = F of (unit -> t)
    let rec f : (unit -> int * [`F of 'a]) as 'a = fun () -> (1, `F f)

*)

module type STREAM = sig
  type 'a t

  val empty : 'a t
  val uncons : 'a t -> 'a option * 'a t
  val hd : 'a t -> 'a option
  val tl : 'a t -> 'a t
  val exhausted : 'a t -> bool

  val cons : 'a -> 'a t -> 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val concat : 'a t -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val repeat : 'a -> 'a t
  val take : int -> 'a t -> 'a option list
  val drop : int -> 'a t -> 'a t

end

module Stream : STREAM = struct

  type 'a t = 'a option * (unit -> 'a t)

  let empty : 'a t = 
    let rec ground = fun () -> None, ground in 
    (None, ground)
  let uncons : 'a t -> 'a option * 'a t = fun (x, y) -> (x, y ())
  let hd : 'a t -> 'a option = function | s -> fst @@ uncons s
  let tl : 'a t -> 'a t = function | s -> snd @@ uncons s
  let exhausted : 'a t -> bool = 
    function | s -> match hd s with | None -> true | _ -> false
  let cons : 'a -> 'a t -> 'a t =  fun x s -> (Some x, fun () -> s)
  let of_list (l : 'a list) : 'a t =
    List.fold_right (fun x s -> (Some x, fun () -> s)) l empty
  let to_list : 'a t -> 'a list = fun s ->
    let rec loop acc = function
      | (Some h, t)  -> loop (h :: acc) (t ())
      | (None, _) -> acc in
    List.rev (loop [] s)
  let rec concat : 'a t -> 'a t -> 'a t = 
    fun u v ->
      match uncons u with
      | None, _ -> v
      | ((Some x) as h, t) ->  (h, fun () -> concat t v)
  let rec map : ('a -> 'b) -> 'a t -> 'b t = 
    fun f -> function
    | None, _ -> (None, fun () -> empty : 'b t)
    | Some x, t -> Some (f x), fun () -> map f (t ())
  let rec repeat : 'a -> 'a t = fun x -> (Some x, fun () -> repeat x)
  let rec take (n : int) (s : 'a t) : 'a option list =
    match (n, s) with
    | (n, _) when n <= 0 -> []
    | (_, (h, t)) -> h :: (take (n - 1) (t ()))
  let rec drop (n : int) (s : 'a t) : 'a t =
    match (n, s)  with
    | (n, _) when n <= 0 -> s
    | (n, (_, t)) -> drop (n - 1) (t ())
end






