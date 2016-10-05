let rec cache_fix 
    (c : ('a, 'b) Hashtbl.t) 
    (f : ('a -> 'b) -> 'a -> 'b) 
    (x : 'a) : 'b =
  try
    Hashtbl.find c x
  with
  | Not_found -> 
    let y = f (cache_fix c f) x in
    Hashtbl.add c x y;
    y
  
let fib = 
  fun n ->
    cache_fix (Hashtbl.create 50)
      (fun f -> function | 0 -> 0 | 1 -> 1 | n -> f (n - 1) + f (n - 2)) n 

let _ = fib 50
