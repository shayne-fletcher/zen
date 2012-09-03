(** Search for a zero of {i f(x)} in the interval {i (a, b)} in
    increments of [dx]. It returns the bounds [Some (x1, x2)] if the
    search was successful and [None] if not. After the first root (the
    root closest to {i a}) has been detected, [search] can be
    called again with {i a} replaced by [x2] in order to find the next
    root. This can be repeated as long as [search] detects a
    root.
*)
let search f a b dx =
  let rec loop x1 f1 x2 f2 =
    if (f1*.f2) <= 0. then
      Some (x1, x2)
    else
      if x1 >= b then
        None
      else
        let x1 = x2 in
        let f1 = f2 in
        let x2 = (x1 +. dx) in
        let f2 = (f x2) in (loop x1 f1 x2 f2)
  in loop a (f a) (a +. dx) (f (a +. dx))
;;
(*
  val search : (float -> float) -> float -> float -> float -> (float * float) option

  let f x = x**3. -. 10.*.(x**2.) +. 5.
  in search f 0. 1. 0.2
  ;;
  - : (float * float) option = Some (0.60000000000000009, 0.8) 

*)

let bisect f bounds eps =
  let (x1, x2) = bounds in
  let f1 = f x1 
  and f2 = f x2
  in
    if f1 = 0.0 then x1
    else
      if f2 = 0.0 then x2
      else
	if f1*.f2 > 0.0 then failwith "Root is not bracketed"
	else
	  let n = int_of_float 
	    (ceil ((log ((abs_float (x2 -. x1)) /. eps)) /. (log 2.0)))
	  in
	    let rec loop i x1 x2  f1 f2 = 
	      if i < 0 then (x1 +. x2) /. 2.0 else
		  let x3 = 0.5 *. (x1 +. x2)  in 
		  let f3 = f x3 in
	            if f3 = 0.0 then x3
		    else
		      if f2 *. f3 <= 0. then
			let x1 = x3
			and f1 = f3
			in loop (i-1) x1 x2 f1 f2
		      else
			let x2 = x3
			and f2 = f3
			in loop (i-1) x1 x2 f1 f2
	      in loop n x1 x2 f1 f2
;;

(*
# let f x = x**3. -. 10.*.(x**2.) +. 5. in bisect f (0.6, 0.8) 1.0e-9;;
- : float = 0.73460350763052706
*)

let newton f f' bounds tol max_its = 
  let (a, b) = bounds in
  let fa = f a in
  if fa = 0. then a
  else
    let fb = f b in
    if fb = 0. then b
    else
      if fa *. fb > 0. then failwith "Root is not bracketed"
      else
  	let rec loop i a b fa =
	  if i = max_its then failwith "Max iterations exceeded" else
	    let x = 0.5 *. (a +. b) in
	    let fx = f x and dfx = (f' x) in
	    let fafx = fa*.fx in
    	    let b = if fafx < 0.0 then x else b in
	    let a = if fafx >= 0.0  then x else a in
	    let fa = if fafx >=0.0 then fx else fa in
	    let dx = (-.fx) /. dfx in
	    let x = x +. dx in
	    let test = (b -.x) *. (x -. a) < 0.0 in
	    let dx = if  test then 0.5*.(b -. a) else dx and 
		x = if test then a +. dx else x in
              if ((abs_float dx) < tol*.(abs_float b)) then x
	      else 
  		  loop (i+1) a b fa
	in loop 0 a b fa
;;
(*
  val newton :
  (float -> float) ->
  (float -> float) -> float * float -> float -> int -> float = <fun>

  # newton (fun x -> x *. x -. 2.) (fun x -> 2.*.x) (1., 3.) 1.0e-9 30;;
  - : float = 1.4142135623730951
*)

