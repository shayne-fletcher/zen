let time (tag:string) f = 
  let t = Sys.time () in
  f ();
  Printf.printf "%s : %f\n" tag (Sys.time () -. t) 
;;

let range s e =
  let rec loop acc s e =
    if s >= e then acc
    else loop (s :: acc) (s + 1) e 
  in List.rev (loop [] s e)
;;

let range2 s e =
  let rec loop acc s e =
    if s >= e then acc
    else loop (acc @ [s]) (s + 1) e 
  in loop [] s e
;;

let rec collect f i n =
  if i = n then ()
  else
    let length = int_of_float (2.0 ** float_of_int i) in
    time (Printf.sprintf "%d, %d" i length)(fun () -> f 0 length) ;
    collect f (i + 1) n
;;

let () = collect range2 10 17;;

