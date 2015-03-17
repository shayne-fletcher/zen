type point =
  { x : int ; y : int }
type size =
  { w : int ; h : int }
type event =
  | Enter | Leave
  | Grab_focus | Loose_focus
  | Down of point | Move of point | Up of point
  | Key of char
exception Redraw
exception Exit

(** The base type of graphical components. *)
class type component = object

  (** The minimal rendering space that the renderer should allow to
      this graphical component for display to be correct. This is the
      unshrinkable space taken by a widget chrome, the minimal size at
      which a picture is readable, etc. It is called at the beginning
      of each rendering pass to check that the window is big
      enough. *)
  method minimum_size : unit -> size

  (** The ideal rendering space that the renderer should allow to this
      graphical component for display to be perfect. The exact text
      size of a text field, the pixel size of a picture, etc. This
      also the size allowed by default when the window is first
      opened. *)
  method natural_size : unit -> size

  (** Tells if the component should be taken into account when
      electing the focused component after a click. *)
  method focusable : bool

  (** At each rendering pass, after the {minimum_size} function is
      called, the main component's layout method is called with the
      origin and window size. These coordinates are the ones to use
      for this rendering pass. The drawing code should not write out
      of this rectangle. Container components should reflect this pass
      order and invariant locally by calling the methods of their
      subcomponents and giving them mutually exclusive
      sub-rectangles. These invariants are however not checked. *)
  method layout : point -> size -> unit

  (** The drawing code. Called at the end of a pass, when {layout} or
      react raised {Redraw}, or on an external event. Should draw
      directly in the graphics window using the rectangle given to
      {layout}. *)
  method draw : unit -> unit

  (** Reacts to an event (return [true]) or ignore it (return [false]).
      Can raise {Redraw} to trigger a display at the end of the pass. *)
  method react : event -> bool

  (** Returns the lists of components under a pointer. The elements
      should be in order of preference for event dispatching. The
      rendering engine will feed mouse events to the {react} methods of
      these components until one of them returns [true]. This is also
      used for focus selection. When a click happens, the first
      {focusable} component will receive the focus. *)
  method find : point -> component list

  (** A utility method to coerce elaborated widgets and container to
      this base component type. *)
  method coerce : component
end

let render (main : unit -> component) =
  let open Graphics in
  open_graph "" ;
  auto_synchronize false ;
  let main = main () in
  let origin = { x = 0 ; y = 0 } in
  let redraw = ref true in
  let exit = ref false in
  let rec loop prev_size prev_hovered prev_focused prev_st =
    let m = main # minimum_size () in
    let size = { w = size_x () ; h = size_y () } in
    if size <> prev_size then redraw := true ;
    if size.w < m.w || size.h < m.h then begin
      resize_window (max m.w size.w) (max m.h size.h) ;
      redraw := true ;
      loop size prev_hovered prev_focused prev_st
    end else begin
      begin try main#layout origin size with Redraw -> redraw := true end ;
      let st = wait_next_event [ Poll ; Key_pressed ] in
      let point = { x = st.mouse_x ; y = st.mouse_y } in
      let hovered = main#find point in
      let focused = ref prev_focused in
      let react o ev =
        try o#react ev with
        | Redraw -> redraw := true ; true
        | Exit -> exit := true ; true in
      List.iter (fun c ->
          if not (List.exists (fun o -> Oo.(id c = id o)) hovered) then
            ignore (react c Leave))
        prev_hovered ;
      List.iter (fun c ->
          if not (List.exists (fun o -> Oo.(id c = id o)) prev_hovered) then
            ignore (react c Enter))
        hovered ;
      let first_to_react ev =
        let rec descend = function
          | o :: os -> if not (react o ev) then descend os
          | [] -> ()
        in descend hovered in
      begin match prev_st, st with
        | { button = false}, { button = true ; mouse_x ; mouse_y } ->
          first_to_react (Down point)
        | { button = true}, { button = false ; mouse_x ; mouse_y } ->
          first_to_react (Up point) ;
          let rec focus = function
            | c :: cs when c#focusable ->
              begin match prev_focused with
                | None -> ignore (react c Grab_focus)
                | Some prev_c when Oo.id c = Oo.id prev_c -> ()
                | Some prev_c ->
                  ignore (react prev_c Loose_focus) ;
                  ignore (react c Grab_focus)
              end ;
              focused := Some c
            | _ :: cs -> focus cs
            | [] ->
              begin match prev_focused with
                | None -> ()
                | Some c ->
                  ignore (react c Loose_focus) ;
              end ;
              focused := None
          in focus hovered
        | { mouse_x = prev_x ; mouse_y = prev_y},
          { mouse_x ; mouse_y } ->
          if mouse_x <> prev_x || mouse_y <> prev_y then
            first_to_react (Move point)
      end ;
      if st.keypressed then begin
        ignore (wait_next_event [ Key_pressed ]) ;
        match !focused with
        | None -> ()
        | Some o -> ignore (react o (Key st.key))
      end ;
      if !redraw then
        (clear_graph () ; main#draw () ; synchronize ()) ;
      ((* OS yield *) try ignore (Unix.select [] [] [] 0.01) with _ -> ()) ;
      redraw := false ;
      if not !exit then loop size hovered !focused st
    end in
  let size = main # natural_size () in
  resize_window size.w size.h ;
  loop size [] None (wait_next_event [ Poll ; Key_pressed ]) ;
  close_graph ()

let padding = 5
let palette =
  Graphics.[| white ;
              rgb 230 230 200 ;
              rgb 210 210 185 ;
              rgb 180 180 160 ;
              rgb 120 120 100 ;
              rgb 50 50 40 ;
              black |]

let raised_border origin size =
  let open Graphics in
  set_color palette.(1) ;
  moveto origin.x origin.y ;
  rlineto 0 size.h ; rlineto size.w 0 ;
  set_color palette.(5) ;
  rlineto 0 (-size.h) ; rlineto (-size.w) 0 ;
  set_color palette.(2) ;
  moveto (origin.x + 1) (origin.y + 1) ;
  rlineto 0 (size.h - 2) ; rlineto (size.w - 2) 0 ;
  set_color palette.(4) ;
  rlineto 0 (-size.h + 2) ; rlineto (-size.w + 2) 0

let lowered_border origin size =
  let open Graphics in
  set_color palette.(5) ;
  moveto origin.x origin.y ;
  rlineto 0 size.h ; rlineto size.w 0 ;
  set_color palette.(1) ;
  rlineto 0 (-size.h) ; rlineto (-size.w) 0 ;
  set_color palette.(4) ;
  moveto (origin.x + 1) (origin.y + 1) ;
  rlineto 0 (size.h - 2) ; rlineto (size.w - 2) 0 ;
  set_color palette.(2) ;
  rlineto 0 (-size.h + 2) ; rlineto (-size.w + 2) 0

class widget = object (self)
  val mutable origin = { x = 0 ; y = 0 }
  val mutable size = { w = 1 ; h = 1 }
  method layout new_origin new_size =
    origin <- new_origin ;
    size <- new_size
  method find { x ; y }  =
    if x >= origin.x && y >= origin.y
       && x <= origin.x + size.w && y <= origin.y + size.h then
      [ self#coerce ]
    else []
  method draw () =
    let open Graphics in
    set_color palette.(3) ;
    fill_rect origin.x origin.y size.w size.h
  method minimum_size () =
    { w = 1 ; h = 1 }
  method natural_size () =
    self # minimum_size ()
  method focusable =
    false
  method react _ =
    false
  method coerce =
    (self :> component)
end
