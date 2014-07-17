(**Depth-first order graph traversal. Generalization of the solution
   accepted to http://ocaml.org/learn/tutorials/99problems.html*)

(**Output signature of the functor [Graph.Make]*)
module type S = sig
  (**Vertex representation*)
  type node

  (**Graph representation*)
  type t

  (**Construct a graph from its adjacency-list representation*)
  val of_adjacency : (node * node list) list -> t

  (**Generate a depth-first order graph traversal sequence*)
  val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
end

module Make (M : Map.OrderedType) : S with type node = M.t
(** Functor building an implementation of the graph structure given a
    totally ordered type *)
