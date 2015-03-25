module Russo = struct

  (*An abstract type of integer streams*)
  module type Stream =
    sig
      type state
      val start : state
      val next : state -> state
      val value : state -> int
    end

  (*An instance of the stream abstract type : the natural numbers
    starting from 2*)
  module Two_onwards : Stream =
    struct
      type state = int
      let start = 2
      let next = fun (i : int) -> succ i
      let value = fun (i : int) -> i
    end

  (*In this implementation of the Sieve of Eratosthenes, the state is
    a stream and the initial state is the stream of natural numbers
    starting from 2*)
  module type State = Stream
  module Start = (Two_onwards : Stream)

  (*One step of the algorithm*)
  module Next (S : State) : State =
    struct
      type state = S.state
      let rec filter : state -> state =
        fun (s : state) ->
          if (S.value s) mod (S.value S.start) = 0
          then filter (S.next s)
          else s
      let start = filter S.start
      let next = fun (s : state) -> filter (S.next s)
      let value = S.value
    end

  (*The sieve, a stream of streams:

   Two_onwards, Next (Two_onwards), Next (Next (Two_onwards)), ...
  *)
  module Sieve : Stream =
    struct
      type state = (module Stream)
      let start = (module Two_onwards : Stream)
      let next = fun (s : state) ->
        let module S = (val s : Stream) in
        (module Next (S) : Stream)
      let value = fun (s : state) ->
        let module S = (val s : Stream) in
        S.value S.start
    end

  (*The nth state of the sieve algorithm*)
  let rec nthstate : int -> Sieve.state =
    fun (n : int) ->
      if n = 0
      then Sieve.start
      else Sieve.next (nthstate (pred n))

  (*The nth prime (where 2 is the zeroth)*)
  let nthprime = fun (n : int) -> Sieve.value (nthstate n)

end
