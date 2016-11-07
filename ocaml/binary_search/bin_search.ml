let bin_search (x : int) : int list -> int option = function
  | [] -> None
  | xs ->
    let rec loop (l, u) =
      if l > u then None 
      else
        let k = (l + u) / 2 in
        match compare x (List.nth xs k)  with
        |  0 -> Some k
        | -1 -> loop (0, k - 1)
        |  1 -> loop (k + 1, u) in
    loop (0, List.length xs - 1)

