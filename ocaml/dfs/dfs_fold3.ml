module type GRAPH = sig
  type node
  type t
  val of_adjacency : (node * node list) list -> t
  val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
end

module Make (M : Map.OrderedType) : GRAPH with type node = M.t = struct

  module Node_map = Map.Make (M)
  type node = M.t
  type t = (node list) Node_map.t

  let of_adjacency l = 
    List.fold_right (fun (x, y) -> Node_map.add x y) l Node_map.empty

  type colors = White|Gray|Black

  type 'a state = {
    d : int Node_map.t ; (*discovery time*)
    f : int Node_map.t ; (*finishing time*)
    pred : node Node_map.t ; (*predecessor*)
    color : colors Node_map.t ; (*vertex colors*)
    acc : 'a ; (*user specified type used by 'fold'*)
  }

  let dfs_fold g c fn acc =
    let rec dfs_visit t u {d; f; pred; color; acc} =
      let edge (t, state) v =
        if Node_map.find v state.color = White then
          dfs_visit t v {state with pred=Node_map.add v u state.pred;}
        else  (t, state)
      in
      let t, {d; f; pred; color; acc} =
        let t = t + 1 in
        List.fold_left edge
          (t, {d=Node_map.add u t d; f;
               pred; color=Node_map.add u Gray color; acc = fn acc u})
          (Node_map.find u g)
      in
      let t = t + 1 in
      t , {d; f=(Node_map.add u t f); pred;
           color=Node_map.add u Black color; acc}
    in
    let v = List.fold_left (fun k (x, _) -> x :: k) []
                           (Node_map.bindings g) in
    let initial_state= 
      {d=Node_map.empty;
       f=Node_map.empty;
       pred=Node_map.empty;
       color=List.fold_right (fun x->Node_map.add x White) v Node_map.empty;
       acc=acc}
    in
    (snd (dfs_visit 0 c initial_state)).acc
end

module Graph : GRAPH with type node = char = Make (Char)

let g = Graph.of_adjacency
          ['u', ['v'; 'x'];
           'v',      ['y'];
           'w', ['z'; 'y'];
           'x',      ['v'];
           'y',      ['x'];
           'z',      ['z'];
          ];;

List.rev (Graph.dfs_fold g 'w' (fun acc c -> c :: acc) [])
