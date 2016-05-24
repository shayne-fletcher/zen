module type STREAM = sig
  type 'a t

  val empty : 'a t
  val exhausted : 'a t -> bool
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val cons : 'a * (unit -> 'a t) -> 'a t
  val uncons : 'a t -> ('a * 'a t)
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list

  val print : (Format.formatter -> 'a -> unit) -> (Format.formatter) -> 'a t -> unit
end

module Stream : STREAM = struct
  type 'a t = Nil | Cons of 'a * (unit -> 'a t)

  let empty : 'a t = Nil

  let exhausted : 'a t -> bool = function | Nil -> true | _ -> false

  let cons : 'a * (unit -> 'a t) -> 'a t = fun (h, t) -> Cons (h, t)

  let of_list (l : 'a list) : 'a t =
    List.fold_right (fun x s -> Cons (x, fun () -> s)) l empty
  
  let hd : 'a t -> 'a = 
    function | Nil -> failwith "hd" | Cons (h, _) -> h

  let tl : 'a t -> 'a t = 
    function | Nil -> failwith "tl" | Cons (_, t) -> t ()

  let uncons : 'a t -> ('a * 'a t) = fun s -> (hd s, tl s)

  let rec iter : ('a -> unit) -> 'a t -> unit =
    fun f -> function | Nil -> () | Cons (h, t) -> f h; iter f (t ())

  let rec to_list : 'a t -> 'a list = function
    | Nil -> []
    | Cons (h, t) -> h :: (to_list (t ()))

  let print 
      (f : Format.formatter -> 'a -> unit) 
      (fmt : Format.formatter) (s : 'a t) : unit =
    let open Format in
    begin
      pp_print_list ~pp_sep:pp_print_space f fmt (to_list s);
      pp_print_newline fmt ()
    end

end

module type SLICE = functor (S : STREAM) -> 
  sig
    type slice = 
    | Slice_all 
    | Slice_one of int
    | Slice_many of int list
    | Slice_from of int
    | Slice_from_counted of int * int
    | Slice_range_incl of int * int
    | Slice_range_excl of int * int
    | Slice_to_incl of int
    | Slice_to_excl of int

    type stream = int S.t

    val slice_all : unit -> stream
    val slice_one : int -> stream
    val slice_from : int -> stream
    val slice_many : int list -> stream
    val slice_from_counted : int -> int -> stream
    val slice_range_incl : int -> int -> stream
    val slice_range_excl : int -> int -> stream

    val print : (Format.formatter) -> stream -> unit

 end

module Slice : SLICE = functor (S : STREAM) -> struct
  type stream = int S.t

  type slice = 
   | Slice_all 
   | Slice_one of int
   | Slice_many of int list
   | Slice_from of int
   | Slice_from_counted of int * int
   | Slice_range_incl of int * int
   | Slice_range_excl of int * int
   | Slice_to_incl of int
   | Slice_to_excl of int

  let slice_many (l : int list) : stream = S.of_list l

  let rec slice_from (first : int) : stream = 
    if first + 1 < first then S.empty else
    S.cons (
      first
    , fun () -> slice_from (first + 1)
    )
  
  let slice_all () : stream = slice_from 0
  
  let slice_one (x : int) : stream =
    S.cons (x, fun () -> S.empty)
  
  let rec slice_from_counted (first : int) (count : int) : stream =
    if count <= 0 then S.empty else
      S.cons (
        first
      , fun () -> slice_from_counted (first + 1) (count - 1)
      )
  
  let rec slice_range_incl (first : int) (last : int) : stream =
    if first > last then S.empty else
      S.cons (
        first
      , fun () -> slice_range_incl (first + 1) last
      )
  
  let rec slice_range_excl (first : int) (last : int) : stream =
    if first >= last then S.empty else
      S.cons (
        first
      , fun () -> slice_range_excl (first + 1) last
      )
        
  let print (fmt : Format.formatter) (s : stream) : unit = 
    S.print (fun fmt x -> Format.fprintf fmt "%d" x) fmt s

end

(*Scratch*)

open Stream
module S = Slice(Stream)
let stdout : Format.formatter = Format.std_formatter

let () = S.print stdout (S.slice_from_counted 0 20)
let () = S.print stdout (S.slice_range_incl 0 20)
let () = S.print stdout (S.slice_range_excl 0 20)
