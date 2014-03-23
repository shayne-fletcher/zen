let _ = 
  let t = Bdate.local_day ()
  and u = Bdate.mk_date (2015, 01, 01) in
  let yr, mon, day= Bdate.year_month_day u in
  Printf.printf "%s\n" (Bdate.string_of_date t);
  Printf.printf "%s\n" (Bdate.string_of_date u);
  Printf.printf "%d\n" (compare u t);
  Printf.printf "%d\n" (compare t u);
  Printf.printf "%d\n" (compare t t);
  Printf.printf "%d\n" (compare u u);
  Printf.printf "%d/%d/%d\n" yr mon day
 
