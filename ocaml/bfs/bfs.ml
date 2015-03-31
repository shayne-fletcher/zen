module type Graph_sig = sig

  type node
  type t
  type colors = White | Gray | Black
  type 'a state

  val of_adjacency : (node * node list) list -> t
  val breadth_first_fold : t -> node -> 'b -> f : ('b -> node -> 'b) -> 'b state

  val colors_of_state : 'a state -> (node * colors) list
  val discovery_of_state : 'a state -> (node * int) list
  val value_of_state : 'a state -> 'a

end

module type GRAPH = sig

  module type Ord=sig 
    type t val compare : t -> t -> int
  end

  module type S = sig
    include Graph_sig
  end

  module Make : functor (N : Ord) -> S with type node = N.t
end

module Graph : GRAPH = struct

  module type Ord = sig 
    type t val compare : t -> t -> int
  end

  module type S = sig
    include Graph_sig
  end

  module Make (M : Ord) : S with type node = M.t = struct
    type node = M.t
    module Node_map : Map.S with type key = node = Map.Make (M)
    type t = (node list) Node_map.t (*Adjacency list*)

    type colors = White | Gray | Black

    type 'a state = {
      d : int Node_map.t ; (*discovery time*)
      pred : node Node_map.t ; (*predecessor*)
      color : colors Node_map.t ; (*vertex colors*)
      acc : 'a ; (*user specified type used by 'fold'*)
    }

    let of_adjacency (l : (node * node list) list) : t = 
      List.fold_right (fun ((x : node), (y : node list)) -> Node_map.add x y) l Node_map.empty

    let breadth_first_fold (g : t) (s : node) (init : 'b) ~(f : 'b -> node -> 'b) : 'b state =
      let q : (node Queue.t) = Queue.create () in
      let v : node list = List.fold_left (fun k (x, _) -> x :: k) [] (Node_map.bindings g) in
      let state : 'a state ref =  ref
        {d=List.fold_right (fun x -> Node_map.add x 0) v Node_map.empty ;
         pred=Node_map.empty;
         color=List.fold_right (fun x -> Node_map.add x White) v Node_map.empty;
         acc=f init s} in
      Queue.add s q;
      while not (Queue.is_empty q) do
        let u : node = Queue.peek q in
          let f (state : 'a state) (v: node) =
            if Node_map.find v state.color = White then
              let () = Queue.add v q in
              let t : int = (Node_map.find u state.d) + 1 in
              {d = (Node_map.add v t state.d); pred=Node_map.add v u state.pred; color=Node_map.add v Gray state.color; acc = f state.acc v}
            else
              state
          in
          state := List.fold_left f (!state) (Node_map.find u g) ;
          let _ : node =  Queue.pop q in
          state := {!state with color=Node_map.add u Black !state.color}
      done;
    !state

    let value_of_state (s : 'a state) : 'a = s.acc

    let discovery_of_state (s : 'a state) : (node * int) list =
      List.rev (Node_map.fold (fun k d acc -> (k, d) :: acc) (s.d) [])

    let colors_of_state (s : 'a state) : (node * colors) list =
      List.rev (Node_map.fold (fun k d acc -> (k, d) :: acc) (s.color) [])

  end

end

(*Type abbreviations*)
type 'a ord = (module Graph.Ord with type t = 'a)
type 'a graph = (module Graph.S with type node = 'a)

(*Function to create a module for an ordered type*)
let mk_ord : 'a. unit -> 'a ord =
  fun (type s) () ->
    (module 
     struct 
       type t = s 
       let compare = Pervasives.compare 
     end : Graph.Ord with type t = s
    )

(*Function to make a graph given a module of an ordered type*)
let mk_graph : 'a. 'a ord -> 'a graph =
  fun (type s) ord ->
    let module Ord = (val ord : Graph.Ord with type t = s) in
    (module Graph.Make (Ord) : Graph.S with type node = s)

(*[breadth_first_fold] as a free function*)
let breadth_first_fold (type a) 
    ~(g : a graph) ~(adj : (a * a list) list) 
    ~(start : a) ~(init : 'b) ~(f : 'b -> a -> 'b) : 'b =
  let module G : Graph.S with type node = a = 
        (val g : Graph.S with type node = a) in
  let inst : G.t = G.of_adjacency adj in
  G.value_of_state (G.breadth_first_fold inst start init ~f)

(* Test *)

(*Test 'modules via functions'*)

let g : char graph = mk_graph (mk_ord ())
let adj : (char * char list) list =
    ['r', ['v'; 's']      ;
     'v', ['r']           ;
     's', ['r'; 'w']      ;
     'w', ['x'; 't']      ;
     't', ['w'; 'x'; 'u'] ;
     'x', ['w'; 't'; 'y'] ;
     'u', ['t'; 'y']      ;
     'y', ['x'; 'u']      ;
    ]
let l : char list = List.rev (
  breadth_first_fold ~g ~adj ~start:'s' ~init:[] ~f:(fun acc x -> x :: acc))

(*Test the functor directly*)

module G : Graph.S with type node = Char.t = Graph.Make (Char)
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
let s : (G.node list) G.state= G.breadth_first_fold g 's' [] (fun acc x -> x :: acc)
let times : (G.node * int) list = G.discovery_of_state s
let colors : (G.node * G.colors) list = G.colors_of_state s
let l : G.node list = List.rev (G.value_of_state s)
(*#val l : Char_graph.node list = ['s'; 'r'; 'w'; 'v'; 'x'; 't'; 'y'; 'u']*)

    
