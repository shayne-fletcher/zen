Michael Savage

1.

z :: ([a] -> b) -> b
z k = k []

s :: (([a] -> b) -> c) -> ([a] -> b) -> a -> c
s n k x = n (\v -> k (x : v))

p :: ((a -> a) -> b) -> b
p a = a id



2.

(I've forgotten how you derive these properly. Time to dig out my university notes)

p z :: [a]

s z :: ([a] -> b) -> a -> b
p (s z) :: a -> [a]

s (s z) :: ([a] -> b) -> a -> a -> b
p (s (s z)) :: a -> a -> [a]




3.

p z
  = z id
  = id []
  = []


p (s z)
  = (s z) id
  = s z id
  = \x -> z (\v -> id (x : v))
  = \x -> z (\v -> x : v)
  = \x -> (\v -> x : v) []
  = \x -> x : []
  = \x -> [x]


p (s (s z))
  = p t where t = s (s z)

t k x
  = (s z) (\v -> k (x : v))
  = (\k' x' -> z (\v -> k' (x' : v))) (\v -> k (x : v))
  = (\k' x' -> ((\v -> k' (x' : v)) [])) (\v -> k (x : v))
  = (\k' x' -> (k' [x'])) (\v -> k (x : v))
  = \x' -> (\v -> k (x : v)) [x']
  = \x' -> k (x : [x'])
  = \x' -> k [x, x']

p t
  = t id
  = \x x' -> id [x, x']
  = \x y -> [x, y]



4. p (s^i z) is a function takes i arguments and returns a list containing its arguments in the given order

Joel Bjornson

1:
=================================================
a)
  let z = fun k -> k []   
  =>
  k : a list -> b         
  =>
  z : (a list -> b) -> b

b)
  let s n k x = n (fun v -> k (x :: v)) 
  =>
  x : a                                 
  k : a list -> b                       
  v : a list                            
  =>
  n : (a list -> b) -> c                =>
  =>
  s : ((a list -> b) -> c) -> (a list -> b) -> a -> c 

c)
  let p a = a (fun x -> x)
  =>
  a : (a -> a) -> b
  =>
  p : ((a -> a) -> b) -> b    

2:
=================================================

a)
  z     : (a list -> b) -> b
  p     : ((a -> a) -> b) -> b 
  =>
  p z   : b list

b)
  z       : (a list -> b) -> b
  s       : ((a list -> b) -> c) -> (a list -> b) -> a -> c 
  =>
  s z     : (a list -> b) -> (a -> b)
  p       : ((a -> a) -> b) -> b 
  =>
  p (s z) : a -> a list

c)
  s z         : (a list -> b) -> (a -> b)                       
  s           : ((a list -> b) -> c) -> (a list -> b) -> a -> c  
  s (s z)     : (a list -> b) -> a -> a -> b                      
  p           : ((a -> a) -> b) -> b 
  =>
  p (s (s z)) : a -> a -> a list 


3:
=================================================
a)
  p z                           = 
  z (fun x -> x)                =
  (fun k -> k []) (fun x -> x)  =
  (fun x -> x) []               =
  []

b)
  p (s z)                                                   =
  p (s (fun k -> k [])                                      =
  p (fun n k x -> n (fun v -> k (x :: v)) (fun k -> k []))  =
  p (fun k x -> (fun k -> k []) (fun v -> k (x :: v)))      =
  p (fun k x -> ((fun v -> k (x :: v))) [])                 =
  p (fun k x -> ((k (x :: []))))                            =
  (fun k x -> ((k (x :: [])))) (fun x -> x)                 =
  (fun x -> (((fun y -> y) (x :: []))))                     =
  fun x -> x :: []

c)
  s (fun k x -> ((k (x :: []))))                                          =
  p (fun n k x -> n (fun v -> k (x :: v)) (fun k x -> ((k (x :: [])))))   =
  p (fun k1 x1 -> (fun k x -> ((k (x :: [])))) (fun v -> k1 (x1 :: v)))   =
  p (fun k1 x1 -> (x -> (((fun v -> k1 (x1 :: v)) (x :: [])))))           =
  p (fun k x1 x2 -> k (x1 :: x2 :: []))                                   =
  (fun k x1 x2 -> k (x1 :: x2 :: []) (fun x -> x)                         =
  (fun x1 x2 -> (fun x -> x) (x1 :: x2 :: [])                             =
  fun x1 x2 -> x1 :: x2 :: []


4:
=================================================
A function of i arguments of the same type, collecting all arguments in a list.
