let dup (x : 'a) : 'a * 'a = (x, x)

type ('a, 'b) product = 'a * 'b

let prod (f : 'c -> 'a) (g : 'c -> 'b) : 'c -> ('a, 'b) product = fun x -> (f x, g x)

let twist ((x : 'a), (y : 'b)) : ('b * 'a) = y, x

let ravel (f : 'u -> 'r) (g : 'v -> 's) : 'u * 'v -> 'r * 's =  fun (x,  y) -> (f x, g y)

type ('a, 'b) sum = | Left of 'a | Right of 'b

let co_product (f : 'a -> 'c) (g : 'b -> 'c) : ('a, 'b) sum -> 'c =  function | Left x -> f x | Right y -> g y

let curry (f : 'a * 'b -> 'c) : 'a -> 'b -> 'c = fun x -> fun y -> f (x, y)

let uncurry (f : 'a -> 'b -> 'c) : 'a * 'b -> 'c = fun (x, y) -> f x y

