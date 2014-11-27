let get_func_time name f = 
  let get_func_time_impl f _ = 
    let before = Sys.time () in 
    f ();
    let after = Sys.time () in 
    let time_taken = (after -. before) in
    time_taken
  in 
  let values = Array.to_list (Array.make 10 0) in
  let times = List.map (get_func_time_impl f) values in 
  let avg l = (List.fold_left ( +. ) 0. l) /. float(List.length l) in
  let avg_time = avg times in 
  Printf.printf "Time taken for [%s] : [%f]\n" name avg_time;
  avg_time
