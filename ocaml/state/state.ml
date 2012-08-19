(* State monad *)

type ('s, 'a) state =
  | State of ('s -> 'a * 's)
  ;;

let run : ('s, 'a) state -> 's -> ('a * 's) =
  fun x ->
     match x with
       | State u -> u
  ;;

let bind : ('s, 'a) state -> ('a -> ('s, 'b) state) -> ('s, 'b) state = 
  fun m k -> 
    State (
      fun state0 -> 
        let (a, state1) =  run m state0 in run (k a) state1
    )
  ;;

let return_ : 'a -> ('s, 'a) state = 
  fun x ->
    State (fun st -> (x, st))
  ;;
  
let (>>=) : ('s, 'a) state -> ('a -> ('s, 'b) state) -> ('s, 'b) state  = 
  fun m k -> bind m k 
;;

(* Basic test. *)

type counter = { v : int ; n : int ; } ;;

let increment_counter : counter -> (int * counter) = 
  fun s ->
    let value = s.v
    in let s' = {v = succ s.v; n = succ s.n }
       in (value, s')
  ;;

let increment_counter_state : (counter, int) state = 
  State increment_counter
  ;;

let res = 
  run 
    (
       increment_counter_state >>= 
        (fun _ -> increment_counter_state)
    )
    {v = 1; n = 2}
in
    match res with
    | (a, s) -> (Printf.printf "{a = %d, v = %d, n = %d}" a s.v s.n) 
    ;;

