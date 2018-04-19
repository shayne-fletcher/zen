let rec insert x = function
  | [] -> [x]
  | h :: tl as ls ->
    if x < h then x :: ls else h :: insert x tl

let rec insertion_sort = function
  | [] | [_] as ls -> ls
  | h :: tl -> insert h (insertion_sort tl)

let bucket_sort a =
  let n = Array.length a in
  let b = Array.make n [] in
  Array.iter
    (fun x ->
       let i =
         int_of_float (
           floor (float_of_int n *. x)
         ) in
        Array.set b i (x :: Array.get b i)
      ) a;
  Array.iteri
    (fun i l ->
       Array.set b i (insertion_sort l)
    ) b;
  Array.fold_left (fun acc bucket -> acc @ bucket) [] b
;;
bucket_sort [| 0.78; 0.17; 0.39; 0.26; 0.72; 0.94; 0.21; 0.12; 0.23; 0.68|]
