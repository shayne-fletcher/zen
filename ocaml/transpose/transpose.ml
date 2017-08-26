let rec transpose (ls : 'a list list) : 'a list list  =
  let rec transpose_rec acc = function
  | [] | [] :: _ -> List.rev acc
  | ls -> transpose_rec (List.map (List.hd) ls :: acc) (List.map (List.tl) ls)
  in transpose_rec [] ls

