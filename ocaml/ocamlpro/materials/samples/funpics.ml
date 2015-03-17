(* Functional 2D raster image synthesis, aka functional pixel shaders *)

(*-- Image Renderer ----------------------------------------------------------*)

let render w h f =
  let dx = 2. /. float w and dy = 2. /. float h in
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" w h) ;
  auto_synchronize false ;
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let px = float x *. dx -. 1.
      and py = float y *. dy -. 1. in
      let (r, g, b, a) = f (px, py) in
      set_color @@ rgb
        (int_of_float ((r +. 1. -. a) *. 255.))
        (int_of_float ((g +. 1. -. a) *. 255.))
        (int_of_float ((b +. 1. -. a) *. 255.)) ;
      plot x y
    done
  done ;
  synchronize () ;
  wait_next_event [ Button_down ; Key_pressed ] |> ignore ;
  close_graph ()

(*-- Colors ------------------------------------------------------------------*)

let valid_color (ra, ga, ba, a) =
  0. <= a && a <= 1. &&
  0. <= ra && ra <= a &&
  0. <= ga && ga <= a &&
  0. <= ba && ba <= a

let clamp ra ga ba a =
  let a = max 0. (min a 1.) in
  let ra = max 0. (min ra a) in
  let ga = max 0. (min ga a) in
  let ba = max 0. (min ba a) in
  (ra, ga, ba, a)

let rbg r g b = (r, g, b, 1.)
let rgba r g b a = (r *. a, g *. a, b *. a, a)

let transparent = (0., 0., 0., 0.)

let is_transparent (_,_,_,a) = a = 0.
let is_opaque (_,_,_,a) = a <> 0.

let black = (0., 0., 0., 1.)

let white = (1., 1., 1., 1.)

let red = (1., 0., 0., 1.)

let green = (0., 1., 0., 1.)

let blue = (0., 0., 1., 1.)

let yellow = (1., 1., 0., 1.)

let violet = (1., 0., 1., 1.)

let cyan = (0., 1., 1., 1.)

(*-- Basic Shapes ------------------------------------------------------------*)

let plane color = fun (x, y) -> color

let disk color radius = fun (x, y) ->
  if x *. x +. y *. y < radius *. radius then color else transparent

let square color radius = fun (x, y) ->
  if abs_float x < radius && abs_float y < radius then color else transparent

(*-- Geometric Transformations -----------------------------------------------*)

let at sx sy f = fun (x, y) ->
  f (x -. sx, y -. sy)

let rotate da f = fun (x, y) ->
  let a = atan2 y x in (* = atan (y /. x) *)
  let p = sqrt (y *. y +. x *. x) in
  let new_a = a -. da in
  let new_x = p *. cos new_a
  and new_y = p *. sin new_a in
  f (new_x, new_y)

let scale z f = fun (x, y) ->
  f (x /. z, y /. z)

let rotozoom da z f = fun (x, y) ->
  let a = atan2 y x in
  let p = sqrt (x *. x +. y *. y) in
  let a = a -. da in
  let p = p /. z in
  f (p *. cos a, p *. sin a)

let rotozoom da z f =
  let g = scale z f in
  rotate da g

let compose f g = fun x -> f (g x)
let rotozoom da z = compose (scale z) (rotate da)
let (@@@) f g = fun x -> f (g x)
let rotozoom da z = scale z @@@ rotate da

let repeat w h f = fun (x, y) ->
  let mod_float x m =
    let r = mod_float x m in
    if r >= 0. then r else m +. r in
  f (mod_float (x +. w /. 2.) w -. w /. 2.,
     mod_float (y +. h /. 2.) h -. h /. 2.)

let of_polar f img = fun (x, y) ->
  let (p, a) = f (sqrt (x *. x +. y *. y)) (atan2 y x) in
  img (p *. cos a, p *. sin a)

let waves wl =
  of_polar (fun p a -> (p -. sin (p /. wl) *. 0.5 *. wl, a))

let warp ph n amp =
  of_polar (fun p a -> (p +. sin (ph +. a *. float n) *. amp, a))

(*-- Combinations ------------------------------------------------------------*)

let compose_src src dest = src
let compose_dest src dest = dest

let compose_src_over src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src as c) = src (x, y) in
  if a_src >= 1. then c else
  let (ra_dest, ga_dest, ba_dest, a_dest) = dest (x, y) in
  let f ca_src ca_dest =
    ca_src +. ca_dest *. (1. -. a_src) in
  (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
   a_src +. a_dest -. a_src *. a_dest)

let compose_dest_over src dest = compose_src_over dest src

let compose_src_in src dest = fun (x, y) ->
  let (ra_dest, ga_dest, ba_dest, a_dest) = dest (x, y) in
  if a_dest <= 0. then
    transparent
  else
    let (ra_src, ga_src, ba_src, a_src) = src (x, y) in
    let f ca_src ca_dest = ca_src *. a_dest in
    (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
     a_src *. a_dest)
let compose_dest_in src dest = compose_src_in dest src

let compose_src_out src dest =
  let inv_alpha src = fun (x, y) ->
    let (ra_src, ga_src, ba_src, a_src) = src (x, y) in
    (ra_src, ga_src, ba_src, 1. -. a_src) in
  compose_src_in src (inv_alpha dest)
let compose_dest_out src dest = compose_src_out dest src

let compose_src_atop src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src) = src (x, y) in
  let (ra_dest, ga_dest, ba_dest, a_dest) = dest (x, y) in
  let f ca_src ca_dest = ca_src *. a_dest +. ca_dest *. (1. -. a_src) in
  (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
   a_dest)
let compose_dest_atop src dest = compose_src_atop dest src

let compose_xor src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src) = src (x, y) in
  let (ra_dest, ga_dest, ba_dest, a_dest) = dest (x, y) in
  let f ca_src ca_dest = ca_src *. (1. -. a_dest) +. ca_dest *. (1. -. a_src) in
  (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
   a_src +. a_dest -. 2. *. a_src *. a_dest)


let clamp (r, g, b, a) =
  let a = max 0. (min a 1.) in
  let r = max 0. (min r a) in
  let g = max 0. (min g a) in
  let b = max 0. (min b a) in
  (r, g, b, a)

let compose_plus src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src) = src (x, y) in
  let (ra_dest, ga_dest, ba_dest, a_dest) = dest (x, y) in
  let f ca_src ca_dest = ca_src +. ca_dest in
  clamp
    (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
     a_src +. a_dest)

let compose_mult src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src) = src (x, y) in
  let (ra_dest, ga_dest, ba_dest, a_dest) = dest (x, y) in
  let f ca_src ca_dest =
    ca_src *. ca_dest +. ca_src *. (1. -. a_dest) +. ca_dest *. (1. -. a_src) in
  clamp
    (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
     a_src +. a_dest -. a_src *. a_dest)

let compose_screen src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src) = src (x, y) in
  let (ra_dest, ga_dest, ba_dest, a_dest) = dest (x, y) in
  let f ca_src ca_dest =
    ca_src +. ca_dest -. ca_src *. ca_dest in
  clamp
    (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
     a_src +. a_dest -. a_src *. a_dest)

let compose_overlay src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src) = src (x, y) in
  let (ra_dest, ga_dest, ba_dest, a_dest) = dest (x, y) in
  let f ca_src ca_dest =
    if 2. *. ca_dest <= a_dest then
      2. *. ca_src *. ca_dest
      +. ca_src *. (1. -. a_dest)
      +. ca_dest *. (1. -. a_src)
    else
      ca_src *. ca_dest
      -. 2. *. (a_dest -. ca_dest) *. (a_src *. ca_src)
      +. ca_src *. (1. -. a_dest)
      +. ca_dest *. (1. -. a_src) in
  clamp
    (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
     a_src +. a_dest -. a_src *. a_dest)

let compose_darken src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src as c_src) = src (x, y) in
  let (ra_dest, ga_dest, ba_dest, a_dest as c_dest) = dest (x, y) in
  let f ca_src ca_dest =
    min (ca_src *. a_src) (ca_dest *. a_dest)
    +. ca_src *. (1. -. a_dest)
    +. ca_dest *. (1. -. a_src) in
  clamp
    (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
     a_src +. a_dest -. a_src *. a_dest)

let compose_lighten src dest = fun (x, y) ->
  let (ra_src, ga_src, ba_src, a_src as c_src) = src (x, y) in
  let (ra_dest, ga_dest, ba_dest, a_dest as c_dest) = dest (x, y) in
  let f ca_src ca_dest =
    max (ca_src *. a_src) (ca_dest *. a_dest)
    +. ca_src *. (1. -. a_dest)
    +. ca_dest *. (1. -. a_src) in
  clamp
    (f ra_src ra_dest, f ga_src ga_dest, f ba_src ba_dest,
     a_src +. a_dest -. a_src *. a_dest)

let all_compositions =
  [ compose_src;
    compose_dest;
    compose_src_over;
    compose_dest_over;
    compose_src_in;
    compose_dest_in;
    compose_src_out;
    compose_dest_out;
    compose_src_atop;
    compose_dest_atop;
    compose_xor;
    compose_plus;
    compose_mult;
    compose_screen;
    compose_overlay;
    compose_darken;
    compose_lighten;
  ]

let rec combine_right binop fs =
  match fs with
  | [] -> plane transparent
  | [f] -> f
  | f :: fs -> binop f (combine_right binop fs)
let combine_left binop fs = combine_right binop (List.rev fs)

(*-- Test: combinations ------------------------------------------------------*)

let sq_src = square yellow 0.66 |> at ~-.0.33 (-.0.33)
let sq_dest = square blue 0.66 |> at 0.33 0.33

let () =
  List.iter
    (fun f -> render 200 200 (f sq_src sq_dest))
    all_compositions


(*-- Animation Renderer ------------------------------------------------------*)

let animate per_frame w h f =
  let dx = 2. /. float w and dy = 2. /. float h in
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" w h) ;
  auto_synchronize false ;
  let tzero = Unix.gettimeofday () in
  let rec loop previous mx my =
    clear_graph () ;
    let time = Unix.gettimeofday () in
    let delay = previous +. per_frame -. time in
    if delay > 0. then ignore (Unix.select [] [] [] delay);
    for x = 0 to w - 1 do
      for y = 0 to h - 1 do
        let px = float x *. dx -. 1.
        and py = float y *. dy -. 1. in
        let (r,g,b,a) = f mx my (time -. tzero) (px, py) in
        set_color @@ rgb
          (int_of_float ((r +. 1. -. a) *. 255.))
          (int_of_float ((g +. 1. -. a) *. 255.))
          (int_of_float ((b +. 1. -. a) *. 255.)) ;
        plot x y
      done
    done ;
    synchronize () ;
    let st = wait_next_event [ Button_down ; Key_pressed ; Mouse_motion ; Poll ] in
    if not (st.button || st.keypressed) then
      let mx = float (st.mouse_x - w / 2) *. dx
      and my = float (st.mouse_y - h / 2) *. dy in
      loop time mx my
    else
      close_graph ()
  in loop 0. 0. 0.

(*-- Simple ------------------------------------------------------------------*)

let () =
  render 200 200 @@
  (* compose_xor *)
  compose_src_out
    (square black 0.6 |> at 0. (0.05)
     |> warp 0. 4 0.03)
   (combine_right compose_src_over
       [ disk black 0.1 |> at (-0.2) 0.15 ;
         disk black 0.2 |> at 0. 0. ;
         disk black 0.1 |> at 0.2 0.15 ])

let () =
  animate (1. /. 20.) 200 200 @@ fun mx my t ->
  compose_xor
    (square red 0.6 |> at 0. (0.05)
     |> rotozoom 0. (1. +. sin t *. 0.5)
     |> warp t 4 0.03)
    (compose_src_out
       (combine_right compose_src_over
          [ disk red 0.2 |> at (-0.4) 0.3 ;
            disk red 0.4 |> at 0. 0. ;
            disk red 0.2 |> at 0.4 0.3 ])
       (combine_right compose_src_over
          [ disk red 0.05 |> at (-0.2) 0. ;
            disk red 0.05 |> at 0.2 0. ;
            disk red 0.1 |> at 0. (-0.2) ])
     |> rotozoom 0. (1. -. sin t *. 0.5))
  |> at (-. mx /. 3.) (-. my /. 3.)

(*-- Complex Examples --------------------------------------------------------*)

let fill color f = compose_src_in (plane color) f

let () =
  let pattern =
    combine_right compose_src_over
      [ square black 0.04 |> at (-0.04) (-0.04) ;
        square black 0.04 |> at (+0.04) (+0.04)]
    |> repeat 0.08 0.08
    |> waves 0.4 in
  animate (1. /. 20.) 200 200 @@ fun mx my t ->
  let t = t /. 10. in
  combine_left compose_src_atop
    [ plane black ;
      pattern |> fill (rgba 1. 0. 0. 0.6) |> rotozoom (0.4 -. t) 4. ;
      pattern |> fill (rgba 1. 1. 0. 0.4) |> rotozoom (0.8 +. t /. 2.) 8. ;
      pattern |> fill (rgba 1. 1. 1. 0.2) |> rotozoom t 2. ;
    ] |>
  let ma = atan2 my mx in
  rotozoom ma (max 0.2 (abs_float mx ** 2.))

let () =
  let pattern =
    waves 0.5
      (repeat 0.08 0.08
         (combine_right compose_src_over
            [ disk white 0.01 |> at (-0.02) 0.015 ;
              disk white 0.02 |> at 0. 0. ;
              disk white 0.01 |> at 0.02 0.015 ]
          |> fill (rgba 1. 1. 1. 0.4))) in
  render 200 200 @@
  compose_src_atop
    (combine_right compose_src_over
       [ pattern |> rotozoom 0.3 2.0 ;
         pattern |> rotozoom 0.3 2.1 ;
         pattern |> rotozoom 0.3 2.2 ;
         pattern |> rotozoom 0.3 2.3 ])
    (plane black)

let image_of_bw_image f = fun (x, y) ->
  let lvl = f (x, y) in
  (lvl, lvl, lvl, 1.)

let mask f = fun (x, y) ->
  let (_, _, _, a) = f (x, y) in a

let alpha src mask = fun (x, y) ->
  let (r, g, b, a) = src (x, y) in
  let a' = mask (x, y) in
  (r, g, b, a *. a')
