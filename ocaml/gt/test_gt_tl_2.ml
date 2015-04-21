#load "graph.cma" ;;
open Gt

module G = struct

  include (Directed_graph.Make (Char) : Directed_graph.S with type node = Char.t)

  let g : t = of_adjacency   
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

end

let components : (G.node list) list = G.strongly_connected_components (G.g)
