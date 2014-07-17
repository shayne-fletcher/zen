module Char_graph : Graph.S with type node = char = Graph.Make (Char)

let g = Char_graph.of_adjacency
            ['u', ['v'; 'x'] ;
             'v',      ['y'] ;
             'w', ['z'; 'y'] ;
             'x',      ['v'] ;
             'y',      ['x'] ;
             'z',      ['z'] ;
            ]

let l = List.rev (Char_graph.dfs_fold g 'w' (fun acc c -> c :: acc) []) 
let string_of_list f l = "[" ^ String.concat ";" (List.map f l) ^ "]"
let _ = Printf.printf "%s\n" (string_of_list (fun c -> String.make 1 c) l)

