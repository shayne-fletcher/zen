(**kinematics.ml

  Motion in a straight line when the magnitude of the acceleration is
  constant.
 *)

let k1 a v0 t =
  (**k_1 

    Velocity given time
    v = v0 + at  (*a = constant*)

  *)
  (v0 +. (a *. t))

let k2 a x0 v0 t =
  (**k2
    
    Position given time
    x = x0 + v0t + at^2/2

  *)
  x0 +. (v0 *. t) +. (0.5 *. a *. (t ** 2.0))

let k3 a v0 x x0 = 
  (**k3

    Velocity given displacement
    v^2 =v0^2 + 2a(x - x0)

   *)
  (v0 ** 2.0) +. (2.0 *. a) *. (x -. x0)

let k4 v0 v = 
  (**k4

    Average velocity
    v = (v1 + v2)/2

  *)
  0.5 *. (v +. v0)

(** You are designing an airport for small planes. One kind of
  airplane that might use this airfield must reach a speed before
  takeoff of at least 27.8 m/s (100 km/hr), and can accelerate at 2.00
  m/s^2.  (a) If the runway is 150 m long, can this airplane reach the
  required speed for takeoff?  *)
let _ = 
  let a = 2.0 (*m/s^2*)
  and v0 = 0.0 (*m/s*)
  and x0 = 0.0 (*m*)
  and x = 150.0 (*m*) 
  in
  Printf.printf
    "The maximum speed reached in %f m is %f m/s" 
    x (sqrt (k3 a v0 x x0)) (*~24.5 m*)
  (*This runway length is not sufficient, because the minimum speed is
    not reached*)
