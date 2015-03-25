(*The operations of the stream interface*)
module type Stream_ops =
sig
  type 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
  val from : (int -> 'a) -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(*The type of a stream value. We'll expose the type of elements ['a],
  but leave the type of streams [t] abstract*)
module type Stream =
sig
  type a
  include Stream_ops
  val stream : a t
end

(*A convenient shorthand for the type of stream values*)
type 'a stream = (module Stream with type a = 'a)

(*An implementation of streams as cons cells with lazy tails*)
module Lazy_stream : Stream_ops =
struct
  type 'a t = Cons of 'a * 'a t Lazy.t
  let head ((Cons (h, _) : 'a t)) : 'a = h
  let tail ((Cons (_, lazy t)) : 'a t) : 'a t = t
  let from (f : int -> 'a) : 'a t = 
    let rec mk n = Cons (f n, lazy (mk (n + 1))) in
    mk 0
  let rec map (f : 'a -> 'b) ((Cons (h, lazy t)) : 'a t) : 'b t = Cons (f h, lazy (map f t))
end

(*An implementation of streams as functions*)
module Functional_stream : Stream_ops =
struct
  type 'a t = int -> 'a
  let head (s : 'a t) : 'a = s 0
  let tail (s : 'a t) : 'a t = fun n -> s (n + 1)
  let from (f : int -> 'a) : 'a t = f
  let map (f : 'a -> 'b) (s : 'a t) : 'b t = fun n -> f (s n)
end

(*Create a stream from an implementation of the interface and an
  indexing function*)
let mk_stream : 'a. (module Stream_ops) -> (int -> 'a) -> 'a stream = 
  fun (type s) ops f ->
    (module
        struct
          type a = s
          include (val ops : Stream_ops)
          let stream = from f
        end : Stream with type a = s
    )

(*Expose the map function as a standalone value*)
let map : 'a. ('a -> 'b) -> 'a stream -> 'b stream =
  fun (type s) (type t) f stream ->
    let module Stream = (val stream : Stream with type a = s) in
    (module
    struct
      type a = t
      include (Stream : Stream_ops with type 'a t = 'a Stream.t)
      let stream = Stream.map f Stream.stream
    end : Stream with type a = t)

let hd : 'a. 'a stream -> 'a =
  fun (type s) stream ->
    let module Stream = (val stream : Stream with type a = s) in
    Stream.head Stream.stream
