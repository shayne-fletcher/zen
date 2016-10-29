
let simulate ~switch num_doors =
  (* Randomly assign a door to hide the car behind *)
  let car = Random.int num_doors in
  (* Select random door representing the first guess *)
  let choice = Random.int num_doors in
  (* The host opens all doors but one. If strategy is 'switch',
     car is found unless it's behind the door initially selected.
     If strategy is 'hold', the car is found only if it's behind the
     door initially selected. *)
  if switch then choice <> car else choice = car
  
let run ~switch num_doors num_trials =
  let wins = ref 0 in
  for i = 1 to num_trials do
    wins := !wins  + if simulate ~switch num_doors then 1 else 0
  done;
  float_of_int !wins /. float_of_int num_trials

# run ~switch:true 3 10000000;;
- : float = 0.6668477
