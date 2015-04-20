#load "graph.cma" ;;
open Gt

module G : Directed_graph.S with type node = Char.t = Directed_graph.Make (Char)

let g = G.of_adjacency
            ['u', ['v'; 'x'] ;
             'v',      ['y'] ;
             'w', ['z'; 'y'] ;
             'x',      ['v'] ;
             'y',      ['x'] ;
             'z',      ['z'] ;
            ]

let l = List.rev (
  G.value_of_state (
    G.dfs_fold g 'w' (fun acc c -> c :: acc) (G.initial_state g [])
  )
)

let n =
  List.rev (
    G.value_of_state (
      G.dfs_traverse g ['u'; 'v'; 'w'; 'x'; 'y'; 'z']
        (fun acc c -> c :: acc) (G.initial_state g [])
    )
  )

let m = 
  List.rev (
    G.dfs_trees_fold g ['u'; 'v'; 'w'; 'x'; 'y'; 'z'] (fun acc tree -> tree :: acc) []
  )
