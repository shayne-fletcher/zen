#load "graph.cma" ;;
open Gt

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
let s= G.bfs_fold g 's' (fun acc x -> x :: acc) (G.initial_state g [])
let l : G.node list = List.rev (G.value_of_state s)
(*
let times : (G.node * int) list = G.discovery_of_state s
let colors : (G.node * G.colors) list = G.colors_of_state s
let l : G.node list = List.rev (G.value_of_state s)
*)
