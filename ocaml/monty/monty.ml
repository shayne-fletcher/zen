module Monty = struct

  (*[dtr w p n] where [n] is the number of doors, selects which "door
    to remain" (closed) given the winning door [w] and the player
    chosen door [p]*)
  let rec dtr w p n =
    if p <> w then 
      (*The player hasn't chosen the winning door, so leave that closed*)
      w 
    else
      (*The player chose the winning door, so pick any other door to keep
        closed*)
      if p = 0 then n - 1 else 0

  (*[gen_game d] generates a game with [d] doors and returns a game
    with a winning door, a player selected door and a door to keep
    closed before if the player wants to switch*)
  let gen_game (d : int) : (int * int * int) =
    let w = Random.int d and p = Random.int d in 
    (w, p, dtr w p d)

  let num_wins = ref 0 (*To keep track of scores*)
  type strategy = Hold | Switch (*The type of strategies*)

  (*Play a single game*)
  let play_game (d : int) (s : strategy) : unit =
    let w, p, r = gen_game d in
    match s with
    | Hold -> num_wins := !num_wins + if p = w then 1 else 0
    | Switch -> num_wins := !num_wins + if r = w then 1 else 0

  (*Play a set of [n] games*)
  let play_games (d : int) (n : int) (s : strategy ) : unit = 
    let rec loop (i : int) : unit = 
      if i = n then ()
      else  begin
        play_game d s;
        loop (i + 1)
      end 
    in loop 0

end

open Monty

(*Initialized from the command line*)
let version       = ref false
let num_doors     = ref 0
let num_sims      = ref 0
let read_args () =
  let specification =
    [("-v", Arg.Set version, "Print the version number");
     ("-d", Arg.Set_int num_doors, "Number of doors (>= 3)" );
     ("-n", Arg.Set_int num_sims, "Number of simulations (>= 1)");
    ]
  in Arg.parse specification
  (fun s ->
    Printf.printf "Warning : Ignoring unrecognized argument \"%s\"\n" s)
  "Usage : monty -d <number of doors> -n <number of simulations>"

(*[fabs e] computes the absolute value of [e]*)
let fabs (e : float) : float = if e < 0. then ~-.e else e

(*Driver*)
let () = 
  try
    read_args ();
    if !version then Printf.printf "1.0.0\n"
    else
      let n = !num_sims and d = !num_doors in
      if d < 3 then
        raise (Invalid_argument "Number of doors must be >= than 3");
      if n < 1 then
        raise (Invalid_argument "Number of simulations must be >= 1");
      begin
        (*Hold*)
        num_wins := 0;
        play_games d n Hold;
        Printf.printf "Num wins (hold): %d\n" !num_wins;
        let err=fabs (float_of_int (!num_wins) /. 
                    (float_of_int n) -. 1.0 /. (float_of_int d)) in
        Printf.printf "Error %f\n" err;
        (*Switch*)
        num_wins := 0;
        num_wins := 0;
        play_games d n Switch;
        Printf.printf "Num wins (switch): %d\n" !num_wins;
        let err=fabs (float_of_int (!num_wins) /. 
                   (float_of_int n) -. (float_of_int (d - 1) /. 
                                                (float_of_int d))) in
        Printf.printf "Error %f\n" err ;
      end

  with 
  | Invalid_argument s -> Printf.printf "%s\n" s
