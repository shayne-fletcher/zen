module Dfs = struct

  module type S = sig
    type t
    module Node_map : Map.S with type key = t
    val fold : ('a -> t -> 'a) -> 'a -> (t list) Node_map.t -> 'a
  end

  module Make (M : Map.OrderedType) : S with type t = M.t = struct

      module Node_map = Map.Make (M)

      type t = M.t
      type colors = White|Gray|Black
      type 'a state = {
        d : int Node_map.t ; (*discovery time*)
        f : int Node_map.t ; (*finishing time*)
        pred : M.t Node_map.t ; (*predecessor*)
        color : colors Node_map.t ; (*vertex colors*)
        acc : 'a ; (*user specified type used by 'fold'*)
      }

      let fold fn acc g =
        let node u (t, {d; f; pred; color; acc}) =
          let rec dfs_visit t u {d; f; pred; color; acc} =
            let edge (t, {d; f; pred; color; acc}) v =
              if Node_map.find v color = White then
                dfs_visit t v {d; f; pred=(Node_map.add v u pred); color; acc}
              else  (t, {d; f; pred; color; acc})
            in
            let t, {d; f; pred; color; acc} =
              let t = t + 1 in
              List.fold_left edge
                (t, {d=Node_map.add u t d; f; pred; color=Node_map.add u Gray color; acc})
                (Node_map.find u g)
            in
            let t = t + 1 in
            t , {d; f=(Node_map.add u t f); pred; color=Node_map.add u Black color; acc = fn acc u}
          in
          if Node_map.find u color = White then dfs_visit t u {d; f; pred; color; acc}
          else (t, {d; f; pred; color; acc})
        in
        let v = List.fold_left (fun k (x, _) -> x::k) [] (Node_map.bindings g) in
        let initial_state= 
          {d=Node_map.empty;
           f=Node_map.empty;
           pred=Node_map.empty;
           color=List.fold_right (fun x->Node_map.add x White) v Node_map.empty;
           acc=acc}
        in
        (snd (List.fold_right node v (0, initial_state))).acc

    end
end

module Graph : Dfs.S with type t = char = Dfs.Make (Char)

let () =
  let string_of_list 
      (f:'a -> string) 
      (l:'a list) : string = 
    "[" ^ String.concat ";" (List.map f l) ^ "]"  
  in

  let g =
       List.fold_right
          (fun (x, y) -> Graph.Node_map.add x y)
          ['u', ['v'; 'x'] ;
           'v',      ['y'] ;
           'w', ['z'; 'y'] ;
           'x',      ['v'] ;
           'y',      ['x'] ;
           'z',      ['z'] ;
          ]
          Graph.Node_map.empty
  in

  let l = Graph.fold (fun acc c -> c :: acc) [] g in
  print_endline (string_of_list (fun c -> String.make 1 c) l)
