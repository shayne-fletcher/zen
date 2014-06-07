module Char_map = Map.Make (Char)
type graph = (char list) Char_map.t

module type S = sig
  type 'a state
  val fold : ('a -> char -> 'a) -> 'a -> graph -> 'a
end

module Dfs : S = struct

  type colors = White|Gray|Black

  type 'a state = {
    d : int Char_map.t ; (*discovery time*)
    f : int Char_map.t ; (*finishing time*)
    pred : char Char_map.t ; (*predecessor*)
    color : colors Char_map.t ; (*vertex colors*)
    acc : 'a ; (*user specified type used by 'fold'*)
  }

  let fold fn acc g =
    let node u (t, {d; f; pred; color; acc}) =
      let rec dfs_visit t u {d; f; pred; color; acc} =
        let edge (t, {d; f; pred; color; acc}) v =
          if Char_map.find v color = White then
            dfs_visit t v {d; f; pred=(Char_map.add v u pred); color; acc}
          else  (t, {d; f; pred; color; acc})
        in
        let t, {d; f; pred; color; acc} =
          let t = t + 1 in
          List.fold_left edge
            (t, {d=Char_map.add u t d; f;
                 pred; color=Char_map.add u Gray color; acc})
            (Char_map.find u g)
        in
        let t = t + 1 in
        t , {d; f=(Char_map.add u t f); pred; color=Char_map.add u Black color; acc = fn acc u}
      in
      if Char_map.find u color = White then dfs_visit t u {d; f; pred; color; acc}
      else (t, {d; f; pred; color; acc})
    in
    let v = List.fold_left (fun k (x, _) -> x::k) [] (Char_map.bindings g) in
    let initial_state= 
       {d=Char_map.empty;
        f=Char_map.empty;
        pred=Char_map.empty;
        color=List.fold_right (fun x->Char_map.add x White) v Char_map.empty;
        acc=acc}
    in
    (snd (List.fold_right node v (0, initial_state))).acc

end

(* Test *)

let () =
  let string_of_list 
      (f:'a -> string) 
      (l:'a list) : string = 
    "[" ^ String.concat ";" (List.map f l) ^ "]"  
  in

  let g =
       List.fold_right
          (fun (x, y) -> Char_map.add x y)
          ['u', ['v'; 'x'] ;
           'v',      ['y'] ;
           'w', ['z'; 'y'] ;
           'x',      ['v'] ;
           'y',      ['x'] ;
           'z',      ['z'] ;
          ]
          Char_map.empty
  in

  let l = Dfs.fold (fun acc c -> c :: acc) [] g in
  print_endline (string_of_list (fun c -> String.make 1 c) l)
