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

let bind : float -> float list -> (int*int)  = 
  fun x xs ->
	let count = (List.length xs)
	and (left, right) = equal_range x xs
	in
	 if (left = count) then
	   failwith "x is right of the domain"
	 else
	   if (right = 0) then
	     failwith "x is left of the domain"
	   else if (right = count) then
	     (left, right - 1)
	   else if (left = right) then
	     (left - 1, right)
	   else (left, right)

let linear_interpolation : float list -> float list -> float -> float =
  fun xs ys x ->
	let (i', i) = bind x xs
	in
  	  let (xi', xi) = ((List.nth xs i'), (List.nth xs i))
	  and (yi', yi)  = ((List.nth ys i'), (List.nth ys i))
	  in
   	    let r = (x -. xi) /. (xi' -. xi)
	    in r *. (yi' -. yi) +. yi

let loglinear_interpolation : float list -> float list -> float -> float =
  fun xs ys x ->
	let (i', i) = bind x xs
	in
  	  let (xi', xi) = ((List.nth xs i'), (List.nth xs i))
	  and (yi', yi)  = ((List.nth ys i'), (List.nth ys i))
	  in
   	    let r = (x -. xi) /. (xi' -. xi)
	    and lyi = log yi
	    and lyi' = log yi'
	    in exp ( r*.(lyi' -. lyi) +. lyi )
;;

let abscissae=[1.0 ; 2.0; 4.0]
and ordinates=[2.0 ; 4.0; 8.0]
in
 let lin_interp = linear_interpolation abscissae ordinates
 and log_interp = loglinear_interpolation abscissae ordinates
 in
   Printf.printf "----\n";

   Printf.printf "log_interpolation 1.0 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (log_interp 1.0) ;
   Printf.printf "log_interpolation 1.1 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (log_interp 1.1) ;
   Printf.printf "log_interpolation 1.5 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (log_interp 1.5) ;
   Printf.printf "log_interpolation 2.0 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (log_interp 2.0) ;
   Printf.printf "log_interpolation 3.0 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (log_interp 3.0) ;
   Printf.printf "log_interpolation 3.5 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (log_interp 3.5) ;
   Printf.printf "log_interpolation 3.9 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (log_interp 3.9) ;

   Printf.printf "----\n";

   Printf.printf "linear_interpolation 1.0 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (lin_interp 1.0) ;
   Printf.printf "linear_interpolation 1.5 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (lin_interp 1.5) ;
   Printf.printf "linear_interpolation 2.5 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (lin_interp 2.5) ;
   Printf.printf "linear_interpolation 3.0 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (lin_interp 3.0) ;
   Printf.printf "linear_interpolation 3.5 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (lin_interp 3.5) ;
   Printf.printf "linear_interpolation 3.9 [1.0; 2.0; 4.0] [1.0; 4.0; 8.0] = %f\n" (lin_interp 3.9) ;
;;


