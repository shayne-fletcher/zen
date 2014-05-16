(* Floyd-Warshall

  "Introduction to Algorithms" - Cormen et. al., 1994
*)

module Int_set =
  Set.Make (struct 
    type t = int
    let compare=Pervasives.compare 
  end)

type vertices = Int_set.t
type adjacency_matrix = (int array) array
type graph = { nodes : vertices ; edges : adjacency_matrix }

module Array_util = struct
  let string_of_array f a = 
    "[|" ^ (String.concat ";" (Array.to_list @@ Array.map f a)) ^ "|]"
  let string_of_matrix f m =
    "[|\n" ^ (String.concat ";\n" (Array.to_list @@ Array.map (string_of_array f) m)) ^ "\n|]"
end

let inf = 1000000 (*avoid overflow*)
let string_of_int' i = if i = inf then "inf" else (string_of_int i)

let rec range i j = if i > (j-1) then [] else i :: (range (i + 1) j)

let floyd_warshall weights = 
  let n = Array.length weights in
  let d_k = Array.copy weights in
  let ds = ref [d_k] in
  for k = 1 to n do
    for i = 1 to n do
      for j = 1 to n do
        d_k.(i-1).(j-1) <- min (d_k.(i-1).(j-1)) (d_k.(i-1).(k-1) + d_k.(k-1).(j-1))
      done
    done ;
    ds := (Array.copy d_k) :: !ds 
  done 
  ; !ds

let floyd_warshall' weights =
  let n = Array.length weights in
  let f ((d_last :: _) as acc) k =
    let d_k = Array.make_matrix n n 0 in
    let f (d_k, d_last :: _) i =
      let g (d_k, ((d_last :: _) as l)) j =
        d_k.(i-1).(j-1) <- min (d_last.(i-1).(j-1)) (d_last.(i-1).(k-1) + d_last.(k-1).(j-1)) ;
        (d_k, l)
      in List.fold_left g (d_k, acc) (range 1 (n + 1))
    in let (d_k, acc) = List.fold_left f (d_k, acc) (range 1 (n + 1)) in d_k :: acc
in List.fold_left f [Array.copy weights] (range 1 (n + 1))

let test_26_4 () =
  let g : graph =
  {
    nodes = List.fold_right (Int_set.add) [1; 2; 3; 4; 5] Int_set.empty ;
    edges = 
      [|      (*  1      2      3     4    ;   5    *)
      (* 1 *) [|  0  ;   3  ;   8   ;  inf ; (-4)  |] ;
      (* 2 *) [| inf ;   0  ;  inf  ;   1  ;   7   |] ;
      (* 3 *) [| inf ;   4  ;   0   ;   inf;  inf  |] ;
      (* 4 *) [|  2  ; inf  ;  (-5) ;   0  ;  inf  |] ;
      (* 5 *) [| inf ; inf  ;  inf  ;   6  ;    0  |] ;
      |]
  }
  in 
  let ds = floyd_warshall (g.edges) in
  Printf.printf "%s" (Array_util.string_of_matrix string_of_int (List.hd ds))

let _ = test_26_4 ()
