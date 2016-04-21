(* Graph of f x = exp (-x) * sin (2 * pi * x)

  Program 4.7, Pascal User Manual and Report, Jensen & Wirth
*)

let round (x : float) : int =
  let f, i = 
    let t = modf x in 
    (fst t, int_of_float@@ snd t) in
  if f = 0.0 then i
  else if i >= 0 then
    if f >= 0.5 then i + 1 else i
  else if -.f >= 0.5 then i - 1 else i

let graph (oc : out_channel) : unit =
  (*The x-axis runs vertically...*)
  let s = 32. in (*32 char widths for [y, y + 1]*)
  let h = 34 in (*char position of x-axis*)
  let d = 0.0625 in (*1/16, 16 lines for [x, x + 1]*)
  let c = 6.28318 in (* 2pi *)
  let lim = 32 in
  for i = 0 to lim do
    let x = d *. (float_of_int i) in
    let y = exp (-.x) *. sin (c *. x) in
    let n = round (s *. y) + h in
    for _ = n downto 0 do output_char oc ' '; done;
    output_string oc "*\n"
  done

let () = print_newline (); graph stdout; print_newline ()
