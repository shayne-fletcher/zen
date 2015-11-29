let fabs x = if x > 0. then x else (~-.x)

(*Newton's formula for (P + PQ)**(m / n) with P, Q in R and m, n in
  Z*)
let newton 
   (p : float) 
   (q : float) 
   (m : int) 
   (n : int) 
   (term : int -> float -> bool) =
  let rec loop acc =
    let s, last, i = acc in
    let num = float_of_int (m - ((i - 1) * n)) in
    let denom =  float_of_int (i * n) in
    let ti = (num /. denom) *. last *. q in
    let s' = s +. ti in
    if (term i s') then (i, s')
    else loop (s', ti, (i + 1))
 in
  let y  = (float_of_int m) /. (float_of_int n) in
  let init = p ** y in
  loop (init, init, 1)

(*Approximate sqrt 2 with P = 1, Q = 1, m/n = 1/2*)
let _ = 
  let sqrt2 = sqrt 2.0 in
  newton 1.0 1.0 1 2 (fun i x -> fabs (x -. sqrt2) <= 0.0001)
