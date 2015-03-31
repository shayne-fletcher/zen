
module Russo_polymorphism = struct

  (*A type for stream operations*)
  type 'state stream_ops = {
    start : 'state;
    next : 'state -> 'state;
    value : 'state -> int ;
  }

  (*The type of stream continuations. There is one field [streamk]
    that is a function that takes a ['state stream_ops] to a ['ctxt] for
    any type ['state]*)
  type 'ctxt stream_cont = {
    streamk : 'state. 'state stream_ops -> 'ctxt
  }

  (*The type of a stream. There is one field [stream] that takes a
    ['ctxt stream_cont] to a ['ctxt] for any type ['ctxt]*)
  type stream = {
    stream : 'ctxt. 'ctxt stream_cont -> 'ctxt
  }

  let two_onwards : stream = {
    stream = fun k -> k.streamk {
      start = 2;
      next = (fun (i : int) -> succ i);
      value = (fun (i : int) -> i);
    }
  }

  type state = stream
  let start : state = two_onwards

  let next ss =
    ss.stream {
      streamk =
        fun {start = s_start; next = s_next; value = s_value} ->
          let rec filter =
            fun s ->
              if (s_value s) mod (s_value s_start) = 0
              then filter (s_next s)
              else s in
          {
            stream = fun k -> k.streamk {
              start = filter s_start;
              next = (fun s -> filter (s_next s));
              value = s_value;
            }
          }
    }

  (* The sieve:

        two_onwards, next(two_onwards), next(next(two_onwards)), ...
  *)
  module Sieve =
  struct
    type state = stream
    let start = (two_onwards : stream)
    let next = fun (s:state) -> next s
    let value = fun (s:state) -> s.stream { streamk = fun { start; value } -> value start }
  end
    
  (* The nth state of the sieve algorithm *)
  let rec nthstate : int -> Sieve.state = 
    fun (n:int) ->
      if n = 0
      then Sieve.start
      else Sieve.next (nthstate (pred n))

  (* The nth prime (where 2 is the zeroth) *)
  let nthprime = fun (n:int) -> Sieve.value (nthstate n)

end
