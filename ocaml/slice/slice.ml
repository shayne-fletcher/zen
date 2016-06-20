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
  val concat : 'a t -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t

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

  let rec concat (u : 'a t) (v : 'a t) : 'a t = match u with
    | Nil -> v
    | Cons (h, tl) -> Cons (h, fun () -> concat (tl ()) v)

  let rec map (f : 'a -> 'b) (s : 'a t) : 'b t = match s with
    | Nil -> Nil
    | Cons (h, tl) -> Cons (f h, fun () -> map f (tl ()))

end

module type INT = sig
  type t

  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val compare : t -> t -> int
  val of_int : int -> t

  val print : (Format.formatter) -> t -> unit
end

module type SLICE = functor (I : INT) (S : STREAM) -> 
  sig

    type int_t = I.t
    type stream_t = int_t S.t

    type slice = 
    | Slice_all 
    | Slice_one of int_t
    | Slice_many of int_t list
    | Slice_from of int_t
    | Slice_from_counted of int_t * int_t
    | Slice_range_incl of int_t * int_t
    | Slice_range_excl of int_t * int_t
    | Slice_to_incl of int_t
    | Slice_to_excl of int_t

    val iterator : slice -> stream_t 

    val print : (Format.formatter) -> stream_t -> unit

 end

module Slice : SLICE = functor (I : INT) (S : STREAM) -> struct
  type int_t = I.t
  type stream_t = int_t S.t

  type slice = 
   | Slice_all 
   | Slice_one of int_t
   | Slice_many of int_t list
   | Slice_from of int_t
   | Slice_from_counted of int_t * int_t
   | Slice_range_incl of int_t * int_t
   | Slice_range_excl of int_t * int_t
   | Slice_to_incl of int_t
   | Slice_to_excl of int_t

  let slice_one (x : int_t) : stream_t =  S.cons (x, fun () -> S.empty)
  let slice_many (l : int_t list) : stream_t = S.of_list l

  let rec slice_from (first : int_t) : stream_t = 
    let next = I.add first (I.one) in
    if I.compare first next < 0 then S.empty else
    S.cons (
      first
    , fun () -> slice_from next
    )

  let slice_all () : stream_t = slice_from (I.zero)
  
  let rec slice_from_counted (first : int_t) (count : int_t) : stream_t =
    if I.compare count (I.zero) <= 0 then S.empty else
      S.cons (
        first
      , fun () -> slice_from_counted (I.add first I.one) (I.sub count I.one)
      )
  
  let rec slice_range_incl (first : int_t) (last : int_t) : stream_t =
    if (I.compare first last) > 0 then S.empty else
      S.cons (
        first
      , fun () -> slice_range_incl (I.add first I.one) last
      )
  
  let rec slice_range_excl (first : int_t) (last : int_t) : stream_t =
    if I.compare first last >= 0 then S.empty else
      S.cons (
        first
      , fun () -> slice_range_excl (I.add first I.one) last
      )
        
  let print (fmt : Format.formatter) (s : stream_t) : unit = 
    S.print I.print fmt s

  let iterator (s : slice) : stream_t =
    match s with
   | Slice_all -> slice_all ()
   | Slice_one x -> slice_one x
   | Slice_many l -> slice_many l
   | Slice_from i -> slice_from i
   | Slice_from_counted (i, cnt) -> slice_from_counted i cnt
   | Slice_range_incl (i, j) -> slice_range_incl i j
   | Slice_range_excl (i, j) -> slice_range_excl i j
   | Slice_to_incl j -> slice_range_incl I.zero j
   | Slice_to_excl j -> slice_range_excl I.zero j

end

(*Scratch*)

module Native_int : INT = struct
  type t = int
  let compare = Pervasives.compare
  let zero = 0
  let one = 1
  let of_int i = i
  let add x y = x + y
  let sub x y = x - y
  let print fmt x = Format.fprintf fmt "%d" x
end

module type GSLICE = functor (Int : INT) (Stream : STREAM) -> sig

  type slice = Slice(Int)(Stream).slice
  type int_t = Slice(Int)(Stream).int_t
  type stream_t = Slice(Int)(Stream).stream_t
      
  type gslice =
  | GSlice of slice
  | GSlice_list of gslice list
  | GSlice_iter of (unit -> int_t option)
  | GSlice_map of (int_t -> int_t) * gslice

end

module GSlice = functor (Int : INT) (Stream : STREAM) -> struct

  module Slice = Slice(Int)(Stream)

  type slice = Slice.slice
  type int_t = Slice.int_t
  type stream_t = Slice.stream_t

  type gslice =
  | GSlice of slice
  | GSlice_list of gslice list
  | GSlice_iter of (unit -> stream_t)
  | GSlice_map of (int_t -> int_t) * gslice

  let rec iterator : gslice -> stream_t = function
    | GSlice s -> Slice.iterator s
    | GSlice_list l -> gslist_iterator l
    | GSlice_map (f, gs) -> gsmap_iterator f gs
    | GSlice_iter it -> gslice_iterator it
    | _ -> failwith "GSlice.iterator : Not implemented"
  and gslist_iterator (ls : gslice list) : stream_t = match ls with
    | [] -> Stream.empty
    | (h :: t) -> Stream.concat (iterator h) (gslist_iterator t)
  and gsmap_iterator (f : int_t -> int_t) (gs : gslice) : stream_t =
    Stream.map f (iterator gs)
  and gslice_iterator (it : unit -> stream_t) : stream_t = it ()

end

module S = Slice (Native_int)(Stream)

let stdout : Format.formatter = Format.std_formatter

let () = 
  S.print stdout @@ S.iterator @@
    S.Slice_from_counted (Native_int.of_int 0, Native_int.of_int 20)

let () = 
  S.print stdout @@ S.iterator @@ 
    S.Slice_range_incl (Native_int.of_int 0, Native_int.of_int 20)

let () = 
  S.print stdout @@ S.iterator @@
    S.Slice_range_excl (Native_int.of_int 0, Native_int.of_int 20)

let () = 
  let u = S.Slice_range_excl (Native_int.of_int 0, Native_int.of_int 20)
  and v = S.Slice_range_excl (Native_int.of_int 20, Native_int.of_int 40) in
  let w = Stream.concat (S.iterator u) (S.iterator v) in
  S.print stdout w
