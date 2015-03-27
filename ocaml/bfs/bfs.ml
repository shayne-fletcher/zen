module type Graph_sig = sig
  type node
  type t

  type colors = White | Gray | Black
  type 'a state

  val of_adjacency : (node * node list) list -> t
  val breadth_first_fold : t -> node -> 'b -> ('b -> node -> 'b) -> 'b state

  val discovery_times : 'a state -> (node * int) list
  val colors : 'a state -> (node * colors) list

end

module type GRAPH = sig

  module type Ord=sig 
    type t val compare : t -> t -> int
  end

  module type S = sig
    include Graph_sig
  end

  module Make : functor (
     N : sig type t val compare : t -> t -> int end) -> 
                                        S with type node = N.t
end

module Graph : GRAPH = struct

  module type Ord=sig 
    type t val compare : t -> t -> int
  end

  module type S = sig
    include Graph_sig
  end

  module Make (M : Ord) : S with type node = M.t = struct
    type node = M.t
    module Node_map : Map.S with type key = node = Map.Make (M)

    type t = (node list) Node_map.t

    type colors = White | Gray | Black

    type 'a state = {
      d : int Node_map.t ; (*discovery time*)
      pred : node Node_map.t ; (*predecessor*)
      color : colors Node_map.t ; (*vertex colors*)
      acc : 'a ; (*user specified type used by 'fold'*)
    }

    let of_adjacency (l : (node * node list) list) : t = 
      List.fold_right (fun (x, y) -> Node_map.add x y) l Node_map.empty

    let breadth_first_fold (g : t) (s : node) (init : 'b) (f : 'b -> node -> 'b) : 'b state =
      let q : (node Queue.t) = Queue.create () in
      let v = List.fold_left (fun k (x, _) -> x :: k) [] (Node_map.bindings g) in
      let state : 'a state ref =  ref
        {d=List.fold_right (fun x -> Node_map.add x 0) v Node_map.empty ;
         pred=Node_map.empty;
         color=List.fold_right (fun x -> Node_map.add x White) v Node_map.empty;
         acc=f init s} in
      Queue.add s q;
      while not (Queue.is_empty q) do
        let u = Queue.peek q in
          let f state v =
            if Node_map.find v state.color = White then
              let () = Queue.add v q in
              let t = (Node_map.find u state.d) + 1 in
              {d = (Node_map.add v t state.d); pred=Node_map.add v u state.pred; color=Node_map.add v Gray state.color; acc = f state.acc v}
            else
              state
          in
          state := List.fold_left f (!state) (Node_map.find u g) ;
          let _ =  Queue.pop q in
          state := {!state with color=Node_map.add u Black !state.color}
      done;
    !state

    let discovery_times (s : 'a state) : (node * int) list =
      List.rev (Node_map.fold (fun k d acc -> (k, d) :: acc) (s.d) [])

    let colors (s : 'a state) : (node * colors) list =
      List.rev (Node_map.fold (fun k d acc -> (k, d) :: acc) (s.color) [])

  end

end

module G : Graph_sig with type node = char = Graph.Make (Char)

(* Test *)

let g : G.t =
  G.of_adjacency
    ['r', ['v'; 's']      ;
     'v', ['r']           ;
     's', ['r'; 'w']      ;
     'w', ['x'; 't']      ;
     't', ['w'; 'x'; 'u'] ;
     'x', ['w'; 't'; 'y'] ;
     'u', ['t'; 'y']      ;
     'y', ['x'; 'u']      ;
    ]

let s = G.breadth_first_fold g 's' [] (fun acc x -> x :: acc)
let times = G.discovery_times s
let colors = G.colors s

(*#val l : Char_graph.node list = ['s'; 'r'; 'w'; 'v'; 'x'; 't'; 'y'; 'u']*)
