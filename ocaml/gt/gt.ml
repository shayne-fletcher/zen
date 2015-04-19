(**Root module, [Gt]*)

(**An abbreviation for the map ordered type module signature*)
module type Ord = Map.OrderedType

(**Suitable for graphs without concern for whether they are directed
   or undirected*)
module type Graph_sig = sig

  (**{2 Data structures} *)

  (**{3 Graph representation} *)

  (**The type of a vertex*)
  type node

  (**The type of graphs*)
  type t

  (**{3 Data structures for traversals}*)

  (**Represents the visitation state of a vertex*)
  type colors = 
  | White  (**Not visited*)
  | Gray  (**Visited not finished*)
  | Black (**Finished*)

  (**Traversal state : discovery/finishing times, colors. This is a
     polymorphic record type making it useful for computing values of
     user defined types via folds*)
  type 'a state

  (**{2 Functions}*)

  (**{3 Adjacency list functions} *)

  (**Adjacency list to graph representation*)
  val of_adjacency : (node * node list) list -> t

  (**Graph to adjacency list representation*)
  val to_adjacency : t -> (node * node list) list

  (**{3 Pretty-print functions} *)

  (**Produce a "dotty" compatible string rendering of a graph
     (considered undirected)*)
  val to_dot_graph : (node -> string) -> t -> string

  (**{3 Functions of traversal states} *)

  (**A pre-traversal state*)
  val initial_state : t -> 'a -> 'a state
    
  (**Colors*)
  val colors_of_state : 'a state -> (node * colors) list

  (**Discovery times*)
  val discovery_of_state : 'a state -> (node * int) list

  (**Finishing times*)
  val finishing_of_state : 'a state -> (node * int) list

  (**Predecssor sub-graph*)
  val predecessor_subgraph : 'a state -> (node * node) list

  (**Value*)
  val value_of_state : 'a state -> 'a

  (**{3 Depth-first-search order graph traversal algorithms}*)

  (**Depth-first search order graph traversal over nodes reachable from
     a specific source node*)
  val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a state -> 'a state

  (**Depth-first search order graph traversal over a subset of nodes*)
  val dfs_traverse_subset : t -> node list -> ('a -> node -> 'a) -> 'a state -> 'a state

end

(**A module type of directed graphs*)
module type Directed_graph_sig = sig

  (**Satisfies the module type of graphs*)
  include Graph_sig

  (**{2 Functions on directed graphs}*)

  (**{3 Compute a transpose}*)

  (**The transpose of a directed graph*)
  val transpose : t -> t

  (**{3 Rendering a directed graph}*)

  (**A "dotty" compatible string rendering of a directed graph*)
  val to_dot_digraph : (node -> string) -> t -> string
end

(**The module type of a module containing a functor called [Make] that
   takes an ordered type module to a graph module*)
module type GRAPH=sig

  (**Module type [S] is the module type of graph modules without
     concern for "directedness". In this restricted view, the types
     [node] and [t] are abstract*)
  module type S = sig
    include Graph_sig
  end

  (**[Make] is a functor from modules of ordered types to graph
     modules. We allow the abstractions to be visible in this
     signature to allow for later composition of modules*)
  module Make : functor (M : Ord) -> sig
    type node = M.t
    module Node_map : Map.S with type key = node
    type t = node list Node_map.t
    type colors = White | Gray | Black
    type 'a state = {
      d : int Node_map.t ; (*discovery time*)
      f : int Node_map.t ; (*finishing time*)
      pred : node Node_map.t ; (*predecessor*)
      color : colors Node_map.t ; (*vertex colors*)
      acc : 'a ; (*user specified type used by 'fold'*)
    }
    val initial_state : t -> 'a -> 'a state
    val colors_of_state : 'a state -> (node * colors) list
    val discovery_of_state : 'a state -> (node * int) list
    val finishing_of_state : 'a state -> (node * int) list
    val value_of_state : 'a state -> 'a
    val predecessor_subgraph : 'a state -> (node * node) list
    val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a state -> 'a state
    val dfs_traverse_subset : t -> node list -> ('a -> node -> 'a) -> 'a state -> 'a state
    val vertices : t -> node list
    val of_adjacency : (node * node list) list -> t
    val to_adjacency : t -> (node * node list) list
    val to_dot_graph : (node -> string) -> t -> string
  end
end

(**[Graph] implements [GRAPH]*)
module Graph : GRAPH = struct

  (*Output signature of the functor [Make]*)
  module type S = sig
    include Graph_sig (*Restricted to a view where [node] and [t] are abstract*)
  end

  (*Functor building an implementation of the graph structure given a
    totally ordered type*)
  module Make (M : Ord) = struct

    (*Concrete definitions of the data types required by [GRAPH]*)
    type node = M.t
    module Node_map : Map.S with type key = node = Map.Make (M)
    type t = (node list) Node_map.t
    type colors = White | Gray | Black
    type 'a state = {
      d : int Node_map.t ; (*discovery time*)
      f : int Node_map.t ; (*finishing time*)
      pred : node Node_map.t ; (*predecessor*)
      color : colors Node_map.t ; (*vertex colors*)
      acc : 'a ; (*user specified type used by 'fold'*)
    }

    (*The functions contained in functor [Make]*)

    let vertices (g : t) : node list =
      List.fold_left (fun acc (k, _) -> k :: acc) [] (Node_map.bindings g)

    let initial_state (g : t) (init : 'a) : 'a state =
      let v : node list = vertices g in
      {d=Node_map.empty;
       f=Node_map.empty;
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

    let predecessor_subgraph (s : 'a state) : (node * node) list =
      let f (acc : (node * node) list) (binding : (node * node)) : (node * node) list =
        binding :: acc in
      List.fold_left f [] (Node_map.bindings s.pred)

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
      (snd (dfs_visit 0 c init))

    let dfs_traverse_subset
        (g : t) 
        (vs : node list)
        (fn : 'a -> node -> 'a) 
        (init : 'a state) : 'a state =
      let f (acc : 'a state) (u : node) : 'a state = dfs_fold g u fn acc in
      List.fold_left f init vs

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

(**The module type of a module containing a functor that takes an
   ordered type module to a directed graph module*)
module type DIRECTED_GRAPH = sig

  (**Module type [S] is the module type of directed graph modules. In
     this restricted view, the types [node] and [t] are abstract*)
  module type S = sig
    include Directed_graph_sig
  end

  (**[Make] is a functor from modules of ordered types to directed
     graph modules. We allow the abstractions to be visible in this
     signature to allow for later composition of modules*)
  module Make : functor (M : Ord) -> sig
    (*[GRAPH] types and functions*)
    type node = M.t
    module Node_map : Map.S with type key = node
    type t = node list Node_map.t
    type colors = White | Gray | Black
    type 'a state = {
      d : int Node_map.t ; (*discovery time*)
      f : int Node_map.t ; (*finishing time*)
      pred : node Node_map.t ; (*predecessor*)
      color : colors Node_map.t ; (*vertex colors*)
      acc : 'a ; (*user specified type used by 'fold'*)
    }
    val initial_state : t -> 'a -> 'a state
    val colors_of_state : 'a state -> (node * colors) list
    val discovery_of_state : 'a state -> (node * int) list
    val finishing_of_state : 'a state -> (node * int) list
    val value_of_state : 'a state -> 'a
    val predecessor_subgraph : 'a state -> (node * node) list
    val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a state -> 'a state
    val dfs_traverse_subset : t -> node list -> ('a -> node -> 'a) -> 'a state -> 'a state
    val vertices : t -> node list
    val of_adjacency : (node * node list) list -> t
    val to_adjacency : t -> (node * node list) list
    val to_dot_graph : (node -> string) -> t -> string
    (*[DIRECTED_GRAPH] types and functions*)
    val transpose : t -> t
    val to_dot_digraph : (node -> string) -> t -> string
  end
end

(**[Directed_graph] implements [DIRECTED_GRAPH]*)
module Directed_graph : DIRECTED_GRAPH = struct

  (*Output signature of the functor [Make]*)
  module type S = sig
    include Directed_graph_sig(*Restricted to a view where [node] and [t] are abstract*)
  end

  (*Functor building an implementation of the directed graph structure
    given a totally ordered type*)
  module Make (M : Ord) = struct
    include Graph.Make (M)

    let transpose (g : t) : t =
      (*Find the nodes 'u' that 'v' such that an edge (u, v)
        exists. In the transposed graph exists the edge (v, u)*)
      let edges_from (v : node) : node list =
        List.fold_left (fun acc (u, edges) ->
          if List.mem v edges then u :: acc else acc ) [] (Node_map.bindings g) in
      let tr : t = 
        List.fold_left 
          (fun (acc : t) (v : node) -> Node_map.add v (edges_from v) acc)  
          (Node_map.empty) (vertices g) in
      tr

    (*A 'dot' representation of a graph*)
    let to_dot_digraph (string_of_node : node -> string) (g : t) : string =
      let f (acc : string) ((key : node), (edges : node list)) : string =
        acc ^ (String.concat "" (List.map (fun v -> Printf.sprintf "%s -> %s;" (string_of_node key) (string_of_node v)) edges)) in
      Printf.sprintf "digraph { %s }\n" (List.fold_left f "" (Node_map.bindings g))

  end
end

(*Test*)
module G : Directed_graph.S with type node = Char.t = Directed_graph.Make (Char)

let g : G.t =
  G.of_adjacency
  [
    'a', ['b']           ;
    'b', ['e'; 'f'; 'c'] ;
    'c', ['d'; 'g']      ;
    'd', ['c'; 'h']      ;
    'e', ['a'; 'f']      ;
    'f', ['g']           ;
    'g', ['f'; 'h']      ;
    'h', ['h']           ;
  ]
let h : (char * char list) list = G.to_adjacency g
let i = G.to_adjacency (G.transpose g)


