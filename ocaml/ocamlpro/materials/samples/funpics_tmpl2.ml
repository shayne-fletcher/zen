
type color = {
  ra: float;
  ga: float;
  ba: float;
  a: float;
}

type coordinate = {
  x: float;
  y: float;
}

type image = coordinate -> color

(*-- Image Renderer --------------------------------------------------------*)

let render w h (f : image) =
  let dx = 2. /. float w and dy = 2. /. float h in
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" w h) ;
  auto_synchronize false ;
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let px = float x *. dx -. 1.
      and py = float y *. dy -. 1. in
      let c = f { x = px; y = py } in
      set_color @@ rgb
        (int_of_float ((c.ra +. 1. -. c.a) *. 255.))
        (int_of_float ((c.ga +. 1. -. c.a) *. 255.))
        (int_of_float ((c.ba +. 1. -. c.a) *. 255.)) ;
      plot x y
    done
  done ;
  synchronize () ;
  wait_next_event [ Button_down ; Key_pressed ] |> ignore ;
  close_graph ()

(*-- Basic Colors ----------------------------------------------------------*)

let clamp ra ga ba a =
  let a = max 0. (min a 1.) in
  let ra = max 0. (min ra a) in
  let ga = max 0. (min ga a) in
  let ba = max 0. (min ba a) in
  { ra; ga; ba; a }

let rgb r g b = clamp r g b 1.
let rgba r g b a = clamp (r *. a) (g *. a) (b *. a) a

let transparent = rgba 0. 0. 0. 0.
let black = rgb 0. 0. 0.
let white = rgb 1. 1. 1.
let red = rgb 1. 0. 0.
let green = rgb 0. 1. 0.
let blue = rgb 0. 0. 1.
let yellow = rgb 1. 1. 0.
let violet = rgb 1. 0. 1.
let cyan = rgb 0. 1. 1.

let is_transparent { a; _ } = a = 0.
let is_opaque { a; _ } = a <> 0.

module Funpics = struct

  (*-- Basic Shapes ----------------------------------------------------------*)

  let plane color = fun { x; y; } -> color

  let disk color radius = fun { x; y; } ->
    if x *. x +. y *. y < radius *. radius then color else transparent

  let square color radius = fun { x; y; } ->
    if abs_float x < radius && abs_float y < radius then
      color
    else
      transparent

  (*-- Geometric Transformations ---------------------------------------------*)

  let at sx sy f = fun { x; y; } ->
    f { x = x -. sx; y = y -. sy }

  let scale s f = fun { x; y; } ->
    f { x = x /. s; y =  y /. s }

  let rotate da f = fun { x; y; } ->
    let a = atan2 y x -. da in
    let p = sqrt (x *. x +. y *. y) in
    f { x = p *. cos a; y =  p *. sin a }

  let rotozoom da z f = fun { x; y; } ->
    let a = atan2 y x in
    let p = sqrt (x *. x +. y *. y) in
    let a = a -. da in
    let p = p /. z in
    f { x = p *. cos a; y =  p *. sin a }

  let repeat w h f = fun { x; y; } ->
    let mod_float x m =
      let r = mod_float x m in
      if r >= 0. then r else m +. r in
    f { x = mod_float (x +. w /. 2.) w -. w /. 2.;
        y = mod_float (y +. h /. 2.) h -. h /. 2. }

  let waves wl f = fun { x; y; } ->
    let wl = wl /. 6.28 in
    let a = atan2 y x in
    let p = sqrt (x *. x +. y *. y) in
    let p = p -. sin (p /. wl) *. 0.5 *. wl in
    f { x = p *. cos a; y =  p *. sin a }

  let warp ph n amp f = fun { x; y; } ->
    let a = atan2 y x in
    let p = sqrt (x *. x +. y *. y) in
    let p = p +. sin (ph +. a *. float n) *. amp in
    f { x = p *. cos a; y =  p *. sin a }

  (*-- Combinations ----------------------------------------------------------*)

  let compose_src src dest = src
  let compose_dest src dest = dest

  let apply f src dest a =
    let ra = f src.ra dest.ra in
    let ga = f src.ga dest.ga in
    let ba = f src.ba dest.ba in
    { ra; ga; ba; a }

  let apply_and_clamp f src dest a =
    let ra = f src.ra dest.ra in
    let ga = f src.ga dest.ga in
    let ba = f src.ba dest.ba in
    clamp ra ga ba a

  let compose_src_over src dest = fun { x; y; } ->
    let src = src { x; y; } in
    if src.a >= 1. then
      src
    else
      let dest = dest { x; y; } in
      let f src_ca dest_ca = src_ca +. dest_ca *. (1. -. src.a) in
      apply f src dest (src.a +. dest.a -. src.a *. dest.a)
  let compose_dest_over src dest = compose_src_over dest src

  let compose_src_in src dest = fun { x; y; } ->
    let dest = dest { x; y; } in
    if dest.a <= 0. then
      transparent
    else
      let src = src { x; y; } in
      let f src_ca dest_ca = src_ca *. dest.a in
      apply f src dest (src.a *. dest.a)
  let compose_dest_in src dest = compose_src_in dest src

  let compose_src_out src dest =
    let inv_alpha src = fun { x; y; } ->
      let src = src { x; y; } in
      { src with a = 1. -. src.a } in
    compose_src_in src (inv_alpha dest)
  let compose_dest_out src dest = compose_src_out dest src

  let compose_src_atop src dest = fun { x; y; } ->
    let src = src { x; y; } in
    let dest = dest { x; y; } in
    let f src_ca dest_ca = src_ca *. dest.a +. dest_ca *. (1. -. src.a) in
    apply f src dest dest.a
  let compose_dest_atop src dest = compose_src_atop dest src

  let compose_xor src dest = fun { x; y; } ->
    let src = src { x; y; } in
    let dest = dest { x; y; } in
    let f src_ca dest_ca =
      src_ca *. (1. -. dest.a) +. dest_ca *. (1. -. src.a) in
    apply f src dest (src.a +. dest.a -. 2. *. src.a *. dest.a)

  let compose_plus src dest = fun { x; y; } ->
    let src = src { x; y; } in
    let dest = dest { x; y; } in
    let f src_ca dest_ca = src_ca +. dest_ca in
    apply_and_clamp f src dest (src.a +. dest.a)

  let compose_mult src dest = fun { x; y; } ->
    let src = src { x; y; } in
    let dest = dest { x; y; } in
    let f src_ca dest_ca =
      src_ca *. dest_ca
      +. src_ca *. (1. -. dest.a)
      +. dest_ca *. (1. -. src.a) in
    apply_and_clamp f src dest (src.a +. dest.a -. src.a *. dest.a)

  let compose_screen src dest = fun { x; y; } ->
    let src = src { x; y; } in
    let dest = dest { x; y; } in
    let f src_ca dest_ca =
      src_ca +. dest_ca -. src_ca *. dest_ca in
    apply_and_clamp f src dest (src.a +. dest.a -. src.a *. dest.a)

  let compose_overlay src dest = fun { x; y; } ->
    let src = src { x; y; } in
    let dest = dest { x; y; } in
    let f src_ca dest_ca =
      if 2. *. dest_ca <= dest.a then
        2. *. src_ca *. dest_ca
        +. src_ca *. (1. -. dest.a)
        +. dest_ca *. (1. -. src.a)
      else
        src_ca *. dest_ca
        -. 2. *. (dest.a -. dest_ca) *. (src.a *. src_ca)
        +. src_ca *. (1. -. dest.a)
        +. dest_ca *. (1. -. src.a) in
    apply_and_clamp f src dest (src.a +. dest.a -. src.a *. dest.a)

  let compose_darken src dest = fun { x; y; } ->
    let src = src { x; y; } in
    let dest = dest { x; y; } in
    let f src_ca dest_ca =
      min (src_ca *. src.a) (dest_ca *. dest.a)
      +. src_ca *. (1. -. dest.a)
      +. dest_ca *. (1. -. src.a) in
    apply_and_clamp f src dest (src.a +. dest.a -. src.a *. dest.a)

  let compose_lighten src dest = fun { x; y; } ->
    let src = src { x; y; } in
    let dest = dest { x; y; } in
    let f src_ca dest_ca =
      max (src_ca *. src.a) (dest_ca *. dest.a)
      +. src_ca *. (1. -. dest.a)
      +. dest_ca *. (1. -. src.a) in
    apply_and_clamp f src dest (src.a +. dest.a -. src.a *. dest.a)

  let rec combine_right binop fs =
    match fs with
    | [] -> plane transparent
    | [f] -> f
    | f :: fs -> binop f (combine_right binop fs)
  let combine_left binop fs = combine_right binop (List.rev fs)

end
