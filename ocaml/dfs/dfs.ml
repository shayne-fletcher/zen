(*depth-first-search

 "Introduction to Algorithms" - Cormen et. al., 1994

*)
module Char_set =
  Set.Make (struct 
    type t = char  
    let compare=Pervasives.compare 
  end)

module Char_map =
  Map.Make (struct 
    type t = char 
    let compare=Pervasives.compare 
  end)

type vertices = Char_set.t
type adjacents = (char list) Char_map.t
type graph = { v:vertices; e:adjacents }

module type S = sig
  type state
  val string_of_state : state -> string
  val depth_first_search : graph -> state
end

module Dfs : S = struct

  type state =
    {
      d : int Char_map.t ; (*discovery time*)
      f : int Char_map.t ; (*finishing time*)
      pred : char Char_map.t ; (*predecessor*)
      color : [`White|`Gray|`Black] Char_map.t ; (*vertex colors*)
    }

  let string_of_state {d; f; pred; color} =
    let e_i k i acc = 
      "'"^(String.make 1 k)^"'"^":"
      ^(string_of_int i)^", "^acc  in
    let e_c k v acc =
      "'"^(String.make 1 k)^"'"^":"
      ^"'"^(String.make 1 (Char_map.find k pred))^"'"^", "^acc  in
    "  d = {" ^ Char_map.fold e_i d "" ^ "}\n"^
    "  f = {" ^ Char_map.fold e_i f "" ^ "}\n"^
    "  pred ={" ^ Char_map.fold e_c pred "" ^ "}\n"

  let depth_first_search {v; e} =
    let initial_state v =
      Char_set.fold (
        fun n {d;f;pred;color} -> { d; f; pred; color = Char_map.add n `White color }
      ) v {d=Char_map.empty; f=Char_map.empty; pred=Char_map.empty; color=Char_map.empty} 
    in
    let node u (t, {d; f; pred; color}) =
      let rec dfs_visit t u {d; f; pred; color} =
        let edge (t, {d; f; pred; color}) v =
          if Char_map.find v color = `White then
            dfs_visit t v {d; f; pred=(Char_map.add v u pred); color} 
          else  (t, {d; f; pred; color})
        in
        let t, {d; f; pred; color} = 
          List.fold_left edge 
            (t + 1, {d=Char_map.add u (t + 1) d; f; pred; color=Char_map.add u `Gray color}) 
            (Char_map.find u e) in
        (t + 1) , {d; f=(Char_map.add u (t + 1) f); pred; color=Char_map.add u `Black color}
      in
      if Char_map.find u color = `White then
        dfs_visit t u {d; f; pred; color}
      else (t, {d; f; pred; color})
    in 
    let _, st = Char_set.fold node v (0, (initial_state v)) in st

end

(* Test *)

let test_23_4 ()=
  let g =
    {
      v = List.fold_right 
        (Char_set.add) 
        ['u'; 'v'; 'w'; 'x'; 'y'; 'z'] 
        Char_set.empty 
      ;
      e = List.fold_right 
        (fun (x, y) -> Char_map.add x y) 
        ['u', ['v'; 'x'] ;
         'v',      ['y'] ;
         'w', ['z'; 'y'] ;
         'x',      ['v'] ;
         'y',      ['x'] ;
         'z',      ['z'] ;
        ]
        Char_map.empty
    }
  in
  let s = Dfs.depth_first_search g in
  Printf.printf "%s\n" (Dfs.string_of_state s)

let _ = test_23_4 ()
