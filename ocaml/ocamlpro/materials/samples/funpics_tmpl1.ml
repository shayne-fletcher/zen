
(*-- Image Renderer --------------------------------------------------------*)

let render w h f =
  let dx = 2. /. float w and dy = 2. /. float h in
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" w h) ;
  auto_synchronize false ;
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let px = float x *. dx -. 1.
      and py = float y *. dy -. 1. in
      let (ra, ga, ba, a) = f (px, py) in
      set_color @@ rgb
        (int_of_float ((ra +. 1. -. a) *. 255.))
        (int_of_float ((ga +. 1. -. a) *. 255.))
        (int_of_float ((ba +. 1. -. a) *. 255.)) ;
      plot x y
    done
  done ;
  synchronize () ;
  wait_next_event [ Button_down ; Key_pressed ] |> ignore ;
  close_graph ()
