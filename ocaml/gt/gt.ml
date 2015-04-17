module type Ord = Map.OrderedType

module type Graph_sig = sig
  type node
  type t
  val of_adjacency : (node * node list) list -> t
  val to_adjacency : t -> (node * node list) list
end

module type Directed_graph_sig = sig
  include Graph_sig
  val transpose : t -> t
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
    val of_adjacency : (node * node list) list -> t
    val to_adjacency : t -> (node * node list) list
  end
end

module Graph : GRAPH = struct

  (*Output signature of the functor [Make]*)
  module type S = sig
    include Graph_sig
  end

  (*Functor building an implementation of the graph structure given a
    totally ordered type*)
  module Make (M : Ord) : sig
    type node = M.t
    module Node_map : Map.S with type key = node
    type t = node list Node_map.t
    val of_adjacency : (node * node list) list -> t
    val to_adjacency : t -> (node * node list) list
  end = struct
    type node = M.t
    module Node_map : Map.S with type key = node = Map.Make (M)
    type t = (node list) Node_map.t

    let of_adjacency (l : (node * node list) list) : t = 
      List.fold_right (fun ((x : node), (y : node list)) -> Node_map.add x y) l Node_map.empty

    let to_adjacency (g : t) : (node * node list) list =
      let f (k : node) (v : node list) (acc : (node * node list) list) : (node * node list) list =
        (k, v) :: acc in
      List.rev (Node_map.fold f g [])
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

    val of_adjacency : (node * node list) list -> t
    val to_adjacency : t -> (node * node list) list
    val transpose : t -> t
    val to_dot_digraph : (node -> string) -> t -> string
  end
end

module Directed_graph : DIRECTED_GRAPH = struct

  (*Output signature of the functor [Make]*)
  module type S = sig
    include Directed_graph_sig
  end

  (*Functor building an implementation of the directed graph structure
    given a totally ordered type*)
  module Make (M : Ord) :  sig
    type node = M.t
    module Node_map : Map.S with type key = node
    type t = node list Node_map.t

    val of_adjacency : (node * node list) list -> t
    val to_adjacency : t -> (node * node list) list
    val transpose : t -> t
    val to_dot_digraph : (node -> string) -> t -> string
  end = struct
    include Graph.Make (M)

    let transpose (g : t) : t = 
      let bindings : (node * node list) list = Node_map.bindings g in
      let keys : node list =
        List.fold_left (fun acc (k, _) -> k :: acc) [] bindings in
      let edges_from (v : node) : node list =
        List.fold_left (fun acc (u, edges) -> 
          if List.mem v edges then u :: acc else acc ) [] bindings in
      let f (acc : t) (v : node) : t =
        Node_map.add v (edges_from v) acc in
      List.fold_left f (Node_map.empty) keys

    let to_dot_digraph (string_of_node : node -> string) (g : t) : string =
      let f (acc : string) ((key : node), (edges : node list)) : string =
        acc ^ (String.concat "" (List.map (fun v -> Printf.sprintf "%s -> %s;" (string_of_node key) (string_of_node v)) edges)) in
      Printf.sprintf "digraph { %s }\n" (List.fold_left f "" (Node_map.bindings g))

  end
end

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
let string_of_char (c : char) : string = Bytes.to_string (Bytes.make 1 c)
let digraph : string = G.to_dot_digraph string_of_char g
