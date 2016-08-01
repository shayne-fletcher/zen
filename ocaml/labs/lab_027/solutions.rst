(* Author: Peter Nguyen (R&D)
 * Date: 07/22/2016
 * Week 27 
 * *)

let dup x = x,x

let twist = function x,y -> y,x

let ravel f g = function u,v -> f u, g v

let product f g = fun z -> f z, g z

type ('a, 'b) either = Left of 'a | Right of 'b

let coproduct f g = function Left x -> f x | Right y -> g y


(*Joel Bjornson*)

1
a)
let dup x = (x,x)

b)
let twist (x,y) = (y,x)

c)
let ravel (f,g) (x,y) = (f x, g y)

d)
product x (f,g) = (f x, g x)

2
type ('a, 'b) sum = { run : 'c. ('a -> 'c) -> ('b -> 'c) -> 'c }

a)
let coproduct (s: ('a, 'b) sum) (f ,g) = s.run f g

