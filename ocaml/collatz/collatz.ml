let collatz (n : int) : int list = 
  let rec loop n l = 
    let l' = n :: l in
    if n = 1 then l'
    else if n mod 2 = 0 then loop (n / 2) l'
    else loop ((3 * n) + 1) l' in
  List.rev@@ loop n []
