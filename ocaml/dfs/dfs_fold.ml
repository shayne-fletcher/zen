module type GRAPH = sig
  type node = char
  type t
  val of_adjacency : (node * node list) list -> t
  val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
end

module M : GRAPH = struct

  module Char_map = Map.Make (Char)
  type node = char
  type t = (node list) Char_map.t

  let of_adjacency l = 
    List.fold_right (fun (x, y) -> Char_map.add x y) l Char_map.empty

  type colors = White|Gray|Black

  type 'a state = {
    d : int Char_map.t ; (*discovery time*)
    f : int Char_map.t ; (*finishing time*)
    pred : char Char_map.t ; (*predecessor*)
    color : colors Char_map.t ; (*vertex colors*)
    acc : 'a ; (*user specified type used by 'fold'*)
  }

  let dfs_fold g c fn acc =
    let rec dfs_visit t u {d; f; pred; color; acc} =
      let edge (t, state) v =
        if Char_map.find v state.color = White then
          dfs_visit t v {state with pred=Char_map.add v u state.pred;}
        else  (t, state)
      in
      let t, {d; f; pred; color; acc} =
        let t = t + 1 in
        List.fold_left edge
          (t, {d=Char_map.add u t d; f;
               pred; color=Char_map.add u Gray color; acc = fn acc u})
          (Char_map.find u g)
      in
      let t = t + 1 in
      t , {d; f=(Char_map.add u t f); pred; color=Char_map.add u Black color; acc}
    in
    let v = List.fold_left (fun k (x, _) -> x :: k) [] (Char_map.bindings g) in
    let initial_state= 
      {d=Char_map.empty;
       f=Char_map.empty;
       pred=Char_map.empty;
       color=List.fold_right (fun x->Char_map.add x White) v Char_map.empty;
       acc=acc}
    in
    (snd (dfs_visit 0 c initial_state)).acc
end

(* Test *)

let g = M.of_adjacency
            ['u', ['v'; 'x'] ;
             'v',      ['y'] ;
             'w', ['z'; 'y'] ;
             'x',      ['v'] ;
             'y',      ['x'] ;
             'z',      ['z'] ;
            ]

let l = List.rev (M.dfs_fold g 'w' (fun acc c -> c :: acc) [])
