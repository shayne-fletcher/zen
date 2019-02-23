module type Ord = Map.OrderedType

module type Graph_sig = sig

  type node
  type t

  type colors = | White | Gray| Black
  type 'a state

  val initial_state : t -> 'a -> 'a state
  val colors_of_state : 'a state -> (node * colors) list
  val discovery_of_state : 'a state -> (node * int) list
  val finishing_of_state : 'a state -> (node * int) list
  val predecessor_subgraph_of_state : 'a state -> (node * node) list
  val value_of_state : 'a state -> 'a

  val of_adjacency : (node * node list) list -> t
  val to_adjacency : t -> (node * node list) list
  val to_dot_graph : (node -> string) -> t -> string
  val bfs_fold : t -> node -> ('b -> node -> 'b) -> 'b state -> 'b state
  val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a state -> 'a state
  val dfs_traverse : t -> node list -> ('a -> node -> 'a) -> 'a state -> 'a state
  val dfs_trees_fold : t -> node list -> ('a -> node list -> 'a) -> 'a -> 'a

end

module type Directed_graph_sig = sig

  include Graph_sig

  val transpose : t -> t
  val strongly_connected_components : t -> (node list) list
  val to_dot_digraph : (node -> string) -> t -> string

end

module type GRAPH=sig

  module type S = sig
    include Graph_sig
  end

  module Make : functor (M : Ord) -> sig
    type node = M.t
    module Node_map : Map.S with type key = node
    type t = node list Node_map.t
    type colors = White | Gray | Black
    type 'a state = {
      d : int Node_map.t ;
      f : int Node_map.t ;
      pred : node Node_map.t ;
      color : colors Node_map.t ;
      acc : 'a ;
    }
    val initial_state : t -> 'a -> 'a state
    val colors_of_state : 'a state -> (node * colors) list
    val discovery_of_state : 'a state -> (node * int) list
    val finishing_of_state : 'a state -> (node * int) list
    val predecessor_subgraph_of_state : 'a state -> (node * node) list
    val value_of_state : 'a state -> 'a
    val bfs_fold : t -> node -> ('b -> node -> 'b) -> 'b state -> 'b state
    val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a state -> 'a state
    val dfs_traverse : t -> node list -> ('a -> node -> 'a) -> 'a state -> 'a state
    val dfs_trees_fold : t -> node list -> ('a -> node list -> 'a) -> 'a -> 'a
    val vertices : t -> node list
    val of_adjacency : (node * node list) list -> t
    val to_adjacency : t -> (node * node list) list
    val to_dot_graph : (node -> string) -> t -> string
  end
end

module Graph : GRAPH = struct

  module type S = sig
    include Graph_sig
  end

  module Make (M : Ord) = struct

    type node = M.t
    module Node_map : Map.S with type key = node = Map.Make (M)
    type t = (node list) Node_map.t
    type colors = White | Gray | Black
    type 'a state = {
      d : int Node_map.t ;
      f : int Node_map.t ;
      pred : node Node_map.t ;
      color : colors Node_map.t ;
      acc : 'a ;
    }

    let vertices (g : t) : node list =
      List.fold_left (fun acc (k, _) -> k :: acc) [] (Node_map.bindings g)

    let initial_state (g : t) (init : 'a) : 'a state =
      let v : node list = vertices g in
      {d=List.fold_right (fun x -> Node_map.add x min_int) v Node_map.empty ;
       f=List.fold_right (fun x -> Node_map.add x max_int) v Node_map.empty ;
       pred=Node_map.empty;
       color=List.fold_right (fun x->Node_map.add x White) v Node_map.empty;
       acc=init}

    let discovery_of_state (s : 'a state) : (node * int) list =
      List.rev (Node_map.fold (fun k d acc -> (k, d) :: acc) (s.d) [])

    let finishing_of_state (s : 'a state) : (node * int) list =
      List.rev (Node_map.fold (fun k d acc -> (k, d) :: acc) (s.f) [])

    let colors_of_state (s : 'a state) : (node * colors) list =
      List.rev (Node_map.fold (fun k d acc -> (k, d) :: acc) (s.color) [])

    let value_of_state (s : 'a state) : 'a = s.acc

    let predecessor_subgraph_of_state (s : 'a state) : (node * node) list =
      let f (acc : (node * node) list) (binding : (node * node)) : (node * node) list =
        binding :: acc in
      List.fold_left f [] (Node_map.bindings s.pred)

    let bfs_fold (g : t) (s : node) (f : 'b -> node -> 'b) (init : 'b state) : 'b state =
      let q : (node Queue.t) = Queue.create () in
      let state : 'b state ref = ref {init with acc = f init.acc s} in
      Queue.add s q;
      while not (Queue.is_empty q) do
        let u : node = Queue.peek q in
          let f (state : 'a state) (v: node) =
            if Node_map.find v state.color = White then
              let () = Queue.add v q in
              let t : int = (Node_map.find u state.d) + 1 in
              {f = Node_map.empty; d = (Node_map.add v t state.d); pred=Node_map.add v u state.pred; color=Node_map.add v Gray state.color; acc = f state.acc v}
            else
              state
          in
          state := List.fold_left f (!state) (Node_map.find u g) ;
          let _ : node =  Queue.pop q in
          state := {!state with color=Node_map.add u Black !state.color}
      done;
    !state

    let dfs_fold (g : t) (c : node) (fn : 'a -> node -> 'a) (init : 'a state) : 'a state =
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
       if Node_map.find c init.color = White
       then
         (snd (dfs_visit 0 c init))
       else init

    let dfs_traverse
        (g : t)
        (vs : node list)
        (fn : 'a -> node -> 'a)
        (init : 'a state) : 'a state =
      let f (acc : 'a state) (u : node) : 'a state = dfs_fold g u fn acc in
      List.fold_left f init vs

    let dfs_trees_fold
        (g : t)
        (vs : node list)
        (fn : 'a -> node list -> 'a)
        (init : 'a) : 'a =
      let f ((s : (node list) state), (acc : 'a))
                 (u : node) : (node list state * 'a) =
        let s : (node list) state = dfs_fold g u (fun acc v -> v :: acc) s in
        let tree : node list = List.rev s.acc in
        ({s with acc = []}, if List.length tree = 0 then acc else fn acc tree) in
      let ((_: (node list) state), (trees : 'a)) = List.fold_left f ((initial_state g []), init) vs in
      trees

    let of_adjacency (l : (node * node list) list) : t =
      List.fold_right (fun ((x : node), (y : node list)) -> Node_map.add x y) l Node_map.empty

    let to_adjacency (g : t) : (node * node list) list =
      let f (k : node) (v : node list) (acc : (node * node list) list) : (node * node list) list =
        (k, v) :: acc in
      List.rev (Node_map.fold f g [])

    let to_dot_graph (string_of_node : node -> string) (g : t): string =
      let f (acc : string) ((key : node), (edges : node list)) : string =
        acc ^ (String.concat "" (List.map (fun v -> Printf.sprintf "%s -- %s;" (string_of_node key) (string_of_node v)) edges)) in
      Printf.sprintf "graph { %s }\n" (List.fold_left f "" (Node_map.bindings g))

  end
end

module type DIRECTED_GRAPH = sig

  module type S = sig
    include Directed_graph_sig
  end

  module Make : functor (M : Ord) -> sig
    type node = M.t
    module Node_map : Map.S with type key = node
    type t = node list Node_map.t
    type colors = White | Gray | Black
    type 'a state = {
      d : int Node_map.t ;
      f : int Node_map.t ;
      pred : node Node_map.t ;
      color : colors Node_map.t ;
      acc : 'a ;
    }

    val initial_state : t -> 'a -> 'a state
    val colors_of_state : 'a state -> (node * colors) list
    val discovery_of_state : 'a state -> (node * int) list
    val finishing_of_state : 'a state -> (node * int) list
    val value_of_state : 'a state -> 'a
    val predecessor_subgraph : 'a state -> (node * node) list
    val bfs_fold : t -> node -> ('b -> node -> 'b) -> 'b state -> 'b state
    val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a state -> 'a state
    val dfs_traverse : t -> node list -> ('a -> node -> 'a) -> 'a state -> 'a state
    val dfs_trees_fold : t -> node list -> ('a -> node list -> 'a) -> 'a -> 'a
    val vertices : t -> node list
    val of_adjacency : (node * node list) list -> t
    val to_adjacency : t -> (node * node list) list
    val to_dot_graph : (node -> string) -> t -> string
    val transpose : t -> t
    val strongly_connected_components : t -> (node list) list
    val to_dot_digraph : (node -> string) -> t -> string
  end

end

module Directed_graph : DIRECTED_GRAPH = struct

  module type S = sig
    include Directed_graph_sig
  end

  module Make (M : Ord) = struct
    include Graph.Make (M)

    let transpose (g : t) : t =
      let acc : t = List.fold_left (fun (acc : t) (u : node) -> Node_map.add u [] acc) Node_map.empty (vertices g) in
      let f (acc : t) (u : node) : t =
        let h (acc : t) (v : node) =
          Node_map.add v (u :: (Node_map.find v acc)) acc
        in List.fold_left h acc (Node_map.find u g) in
      List.fold_left f acc (vertices g)

    let strongly_connected_components (g : t) : (node list) list =
      let s : unit state = initial_state g () in
      let ts : (node * int) list = finishing_of_state (dfs_traverse g (vertices g) (fun _ _ -> ()) s) in
      let gt : t = transpose g in
      let cmp (x : node * int) (y : node * int) : int =
        let fx, fy = snd x, snd y in
        if fx < fy then -1 else if fx = fy then 0 else 1 in
      let vs : node list = List.map (fun e -> fst e) (List.rev (List.sort cmp ts)) in
      List.rev (dfs_trees_fold gt vs (fun acc tree -> tree :: acc) [])

    let to_dot_digraph (string_of_node : node -> string) (g : t) : string =
      let components: (node list) list = strongly_connected_components g in
      let f (l : node list) : string =
        "{rank=same; "^(String.concat " " (List.map string_of_node l))^"}" in
      let rank=String.concat "\n" (List.map f components) in
      let f (acc : string) ((key : node), (edges : node list)) : string =
        acc ^ (String.concat "" (List.map (fun v -> Printf.sprintf "%s -> %s;" (string_of_node key) (string_of_node v)) edges)) in
      let s = Printf.sprintf "digraph { %s %s }\n" rank (List.fold_left f "" (Node_map.bindings g)) in
      Printf.printf "%s" s ; s

  end
end
