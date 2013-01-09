let string_of_list f l =
  "[" ^ String.concat ";" (List.map f l) ^ "]"
;;

let string_of_vector f x =
  string_of_list f (Array.to_list x)
;;

let string_of_matrix f a =
  string_of_list
    (string_of_list f)
    (List.map Array.to_list (Array.to_list a))
;;

let make_augmented_coefficient_matrix a b =
  Array.mapi (fun i r -> Array.append r [| b.(i) |]) a
;;

let elimination_phase a =
  let elimination_step n k pr tr =
    let lam = tr.(k) /. pr.(k) in
    let res =
      Array.mapi
	(fun i a ->
	  a -. lam *. pr.(k + i))
	(Array.sub tr k (n - k)) in
    Array.append (Array.make k 0.0) res in
  let n = (Array.length a)in
  for k = 0 to n - 1 do
    for i = k + 1 to n - 1 do
      a.(i) <- elimination_step (n + 1) k a.(k) a.(i)
    done
  done;
  a
;;

let back_substitution_phase a =
  let back_substitution_step n k a x =
    let akk = a.(k).(k)
    and bk = a.(k).(n) in
    if n = 0 then bk /. akk
    else
      let start = k + 1
      and len = (n - k - 1) in
      let s = Array.sub a.(k) start len in
      let t =
	Array.fold_right (+.)
	  (Array.mapi (fun i a -> a *. x.(start + i)) s) 0.0 in
      (bk -. t) /. akk in
  let n = Array.length a in
  let x = Array.make n 0.0 in
  for k = n - 1 downto 0 do
    x.(k) <- back_substitution_step n k a x
  done;
  x
;;

let _ =
  let a =
    [|
      [| 4.0; -2.0; 1.0 |] ;
      [| -2.0; 4.0; -2.0 |] ;
      [| 1.0; -2.0; 4.0 |] ;
    |]
  and b = [| 11.0 ; -16.0 ; 17.0 ; |]in
  let x =
    back_substitution_phase
      (elimination_phase
	 (make_augmented_coefficient_matrix a b)
      )
  in Printf.printf "%s\n" (string_of_vector string_of_float x)
;;
