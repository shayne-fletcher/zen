module SF = struct
let prod l r =
  let g acc a =
    let f acc x =
      (a, x) :: acc
    in List.fold_left f acc r
  in List.fold_left g [] l |> List.rev
  
end

module ZT = struct
let prod a b =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y )) b) a)

end
