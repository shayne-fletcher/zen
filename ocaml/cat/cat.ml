let prod (f : 'c -> 'a) (g : 'c -> 'b) 
  : 'c -> ('a * 'b) = fun x -> (f x, g x)

let twist ((x : 'a), (y : 'b)) : ('b * 'a) = y, x

let ravel (f : 'u -> 'r) (g : 'v -> 's) 
  : 'u * 'v -> 'r * 's =  fun (x,  y) -> (f x, g y)

type ('a, 'b) sum = | Left of 'a | Right of 'b

let co_product (f : 'a -> 'c) (g : 'b -> 'c)
  : ('a, 'b) sum -> 'c =  function | Left x -> f x | Right y -> g y
