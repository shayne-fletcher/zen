type point ={x : float; y : float}

type orientation=
  | Counter_clockwise
  | Clockwise
  | Colinear

let orientation p0 p1 p2 =
  let {x=x0; y=y0} = p0
  and {x=x1; y=y1} = p1
  and {x=x2; y=y2} = p2 in
  let m1 =(y1 -. y0) /. (x1 -. x0)
  and m2 =(y2 -. y1) /. (x2 -. x1) in
  (*
  let exp = (y1 -. y0) *. (x2 -. x1) -. (y2 -. y1) *. (x1 -. x0) in
   if exp < 0. then Clockwise
   else if exp > 0. then Counter_clockwise
   else Colinear
*)
  Printf.printf "m1 : %f\n" m1;
  Printf.printf "m2 : %f\n" m2;
  if m1 < m2 then Counter_clockwise
  else if m1 > m2 then Clockwise
    else Colinear
;;

let p0 = {x=0.; y=0.} in
let p1 = {x=4.; y=4.} in
let p2 = {x=1.; y=2.} in
orientation p0 p1 p2
;;
