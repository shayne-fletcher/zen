(*depth-first-search

  "Introduction to Algorithms" - Cormen et. al., 1994

  This solution is a bit more general than the specification - it's
  easy to adapt.

  (1) This 'depth-first-search' considers every node as a starting
  point. Every node of the graph is visited once (but only once). To
  restrict the search, replace the fold over all vertices of the last
  line
  {[
  snd (List.fold_right node v initial_state)
  ]}
  with a direct call to [node] on the 'source' vertex of interest ([s]
  say)
  {[
  snd (node s initial_state)
  ]}.

  (2) To build the 'list of nodes reachable from the starting point',
  prepend each node to a running list just when its finishing time
  has become known and it's about to be colored black.

  That is...
  {[
  let t = t + 1 in
  (* ... here. *)
  t , {d; f=(Char_map.add u t f); pred; color=Char_map.add u `Black color}
  ]}

*)

module Char_map = Map.Make (Char)

type graph = (char list) Char_map.t

module type S = sig
  type state
  val string_of_state : state -> string
  val depth_first_search : graph -> state
end

module Dfs : S = struct

  type colors = White|Gray|Black

  type state = {
    d : int Char_map.t ; (*discovery time*)
    f : int Char_map.t ; (*finishing time*)
    pred : char Char_map.t ; (*predecessor*)
    color : colors Char_map.t ; (*vertex colors*)
  }

  let string_of_state {d; f; pred; color} =
    let open Printf in
    let bindings m fmt =
      let b = Char_map.bindings m in
      String.concat ", " (List.map (fun (x,y) -> sprintf fmt x y) b) in
    sprintf " d = {%s}\n f = {%s}\n pred = {%s}\n"
      (bindings d "'%c':'%d'") (bindings f "'%c':'%d'")
      (bindings pred "'%c':'%c'")

  let depth_first_search g =
    let node u (t, {d; f; pred; color}) =
      let rec dfs_visit t u {d; f; pred; color} =
        let edge (t, {d; f; pred; color}) v =
          if Char_map.find v color = White then
            dfs_visit t v {d; f; pred=(Char_map.add v u pred); color}
          else  (t, {d; f; pred; color})
        in
        let t, {d; f; pred; color} =
          let t = t + 1 in
          List.fold_left edge
            (t, {d=Char_map.add u t d; f;
                 pred; color=Char_map.add u Gray color})
            (Char_map.find u g)
        in
        let t = t + 1 in
        t , {d; f=(Char_map.add u t f); pred; color=Char_map.add u Black color}
      in
      if Char_map.find u color = White then dfs_visit t u {d; f; pred; color}
      else (t, {d; f; pred; color})
    in
    let v = List.fold_left (fun acc (x, _) -> x::acc) [] (Char_map.bindings g) in
    let initial_state= 
       {d=Char_map.empty;
        f=Char_map.empty;
        pred=Char_map.empty;
        color=List.fold_right (fun x->Char_map.add x White) v Char_map.empty}
    in
    snd (List.fold_right node v (0, initial_state))

end

(* Test *)

let () =
  let g =
       List.fold_right
          (fun (x, y) -> Char_map.add x y)
          ['u', ['v'; 'x'] ;
           'v',      ['y'] ;
           'w', ['z'; 'y'] ;
           'x',      ['v'] ;
           'y',      ['x'] ;
           'z',      ['z'] ;
          ]
          Char_map.empty
  in
  let s = Dfs.depth_first_search g in
  print_endline (Dfs.string_of_state s)
