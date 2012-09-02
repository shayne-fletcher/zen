module Interpolation =
  struct

    let lower_bound : float -> float list -> int = 
      fun x xs ->
	let rec loop = fun first count -> 
	  if count = 0 then
	    first
	  else
	    let half = count/2
	    in 
  	      let mid = first+half
	      in 
	        if (List.nth xs mid) < x then
	          loop (mid+1) (count-half-1)
	        else
	          loop first half
	in
	  loop 0 (List.length xs)
    ;;
 
    let upper_bound : float -> float list -> int = 
     fun x xs ->
       let rec loop = fun first count ->
	 if count = 0 then first
	 else
	   let half=count/2
	   in
  	     let mid=first+half
	     in
	     if x < (List.nth xs mid) then
	       loop first half
	     else
	       loop (mid+1) (count-half-1)
       in
        loop 0 (List.length xs)
    ;;

    let equal_range : float -> float list -> (int*int) = 
      fun x xs ->
	let a = lower_bound x xs
	and b = upper_bound x xs
	in 
          (a, b)
    ;;

  end
;;

open Interpolation ;;

let p = Interpolation.equal_range 3.0 [1.0; 2.0; 4.0] in
  Printf.printf "lower_bound 3.0 [1.0; 2.0; 4.0] = %d\n" (Interpolation.lower_bound 3.0 [1.0; 2.0; 4.0]) ;
  Printf.printf "upper_bound 3.0 [1.0; 2.0; 4.0] = %d\n" (Interpolation.upper_bound 3.0 [1.0; 2.0; 4.0]) ;
  Printf.printf "equal_range 3.0 [1.0; 2.0; 4.0] = (%d, %d)\n" (fst p) (snd p)

