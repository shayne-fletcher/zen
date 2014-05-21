(*depth-first-search

  "Introduction to Algorithms" - Cormen et. al., 1994

  This solution is a bit more general than the specification - it's
  easy to adapt.

  (1) This 'depth-first-search' considers every node as a starting
  point. Every node of the graph is visited once (but only once). To
  restrict the search, replace the fold of the last line
  {[Char_set.fold node v (0, (initial_state v))]} with a direct call
  to [node] on the starting node of interest.

  (2) To build the 'list of nodes reachable from the starting point',
  prepend each node to a running list just when it's finishing time
  has become known and it's about to be colored black.

  That is...
  {[
  let t = t + 1 in
  (* ... here. *)
  t , {d; f=(Char_map.add u t f); pred; color=Char_map.add u `Black color}
  ]}

*)
module Char_set = Set.Make (Char)
module Char_map = Map.Make (Char)

type vertices = Char_set.t
type adjacents = (char list) Char_map.t
type graph = { v:vertices; e:adjacents }

module type S = sig
  type state
  val string_of_state : state -> string
  val depth_first_search : graph -> state
end

module Dfs : S = struct

  type state = {
    d : int Char_map.t ; (*discovery time*)
    f : int Char_map.t ; (*finishing time*)
    pred : char Char_map.t ; (*predecessor*)
    color : [`White|`Gray|`Black] Char_map.t ; (*vertex colors*)
  }

  let string_of_state {d; f; pred; color} =
    let cat = String.concat ", " in
    " d = {"^(cat (List.map (fun (x, y)->Printf.sprintf "'%c':'%d'" x y) (Char_map.bindings d)))^"}\n"^
    " f = {"^(cat (List.map (fun (x, y)->Printf.sprintf "'%c':'%d'" x y) (Char_map.bindings f)))^"}\n"^
    " pred = {"^(cat (List.map (fun (x, y)->Printf.sprintf "'%c':'%c'" x y) (Char_map.bindings pred)))^"}\n"

  let depth_first_search {v; e} =
    let node u (t, {d; f; pred; color}) =
      let rec dfs_visit t u {d; f; pred; color} =
        let edge (t, {d; f; pred; color}) v =
          if Char_map.find v color = `White then
            dfs_visit t v {d; f; pred=(Char_map.add v u pred); color}
          else  (t, {d; f; pred; color})
        in
        let t, {d; f; pred; color} =
          let t = t + 1 in
          List.fold_left edge
            (t, {d=Char_map.add u t d; f;
                 pred; color=Char_map.add u `Gray color})
            (Char_map.find u e)
        in
        let t = t + 1 in
        t , {d; f=(Char_map.add u t f); pred; color=Char_map.add u `Black color}
      in
      if Char_map.find u color = `White then dfs_visit t u {d; f; pred; color}
      else (t, {d; f; pred; color})
    in
    snd (Char_set.fold node v
           (0, {d=Char_map.empty;
                f=Char_map.empty;
                pred=Char_map.empty;
                color=Char_set.fold (fun x -> Char_map.add x `White) v Char_map.empty}))

end

(* Test *)

let () =
  let g = {
    v = List.fold_right
          (Char_set.add)
          ['u'; 'v'; 'w'; 'x'; 'y'; 'z']
          Char_set.empty
    ;
    e = List.fold_right
          (fun (x, y) -> Char_map.add x y)
          ['u', ['v'; 'x'] ;
           'v',      ['y'] ;
           'w', ['z'; 'y'] ;
           'x',      ['v'] ;
           'y',      ['x'] ;
           'z',      ['z'] ;
          ]
          Char_map.empty
  }
  in
  let s = Dfs.depth_first_search g in
  Printf.printf "%s\n" (Dfs.string_of_state s)
