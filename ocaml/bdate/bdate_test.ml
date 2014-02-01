let _ = 
  let t = Bdate.local_day () in
  Printf.printf "The current date is %s\n" (Bdate.string_of_date t)
