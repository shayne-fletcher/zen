(*
  http://okmij.org/ftp/Computation/extra-polymorphism.html

  The problem is to write a function that first takes the numeral
  [n]. The function then takes [n] other arguments and returns them as
  the [n]-elment list. It seems the function should have type [n ->
  ('a ->) ^ n -> 'a list], which of course is not possible in OCaml.
*)

let z = fun k -> k []
(* Assume [[]] to be of type ['a list]. Since [k] is is applied to
   [[]] then [k] must have type ['a list -> 'b] for some ['b]. So,
   [z] is a function in [k], returning what [k] returns, that is,
   [z] is of type [('a list -> 'b) -> 'b].
*)

let s n k x  = n (fun v -> k (x :: v))
(*
  [x] is "consed" onto [v] so, [v] has type [`a list] and [x] type
  ['a]. [k] is then applied to this value of ['a list] yielding a
  value of type ['b] (say) so [k] is of type ['a list -> 'b]. So,
  summarizing have,
  {[
    val x : 'a
    val v : 'a list
    val k : 'a list -> 'b
  ]}

  [n] is applied to a function taking an ['a list -> 'b] so, [n] has
  type [('a list -> b) -> 'c] for some [c] and so finally we may
  write,
  {[
    val s : (('a list -> 'b) -> 'c) -> ('a list -> 'b) -> 'a -> 'c
  ]}
*)

let p a = a (fun x -> x)
(* Let [x] have type ['a]. Then, [a] must have type [('a -> 'a) ->
   'b], so [p] has type [(('a -> 'a) -> 'b) -> 'b].
*)

(*
    Now let's try some things out:

      - What is the type of [p z]? 

        Given,
        {[
            val z : ('a list -> 'b) -> 'b
            val p : (('a -> 'a) -> 'b) -> 'b
        ]}

        by unification, it must be ['a list]. 

        By inspection, 
        [{
          p z = z (fun x -> x) = (fun k -> k []) (fun x -> x) = []
        ]}

        the empty list.

      - What is the type of [p (s z)]?

       Given,
        {[
            val z : ('a list -> 'b) -> 'b
            val s : (('a list -> 'b) -> 'c) -> ('a list -> 'b) -> 'a -> 'c
            val p : (('a -> 'a) -> 'b) -> 'b
        ]}

      consider the expression [s z]. By unification, we have ['b] =
      ['c] and [s z] = [('a list -> 'b) -> 'a -> 'b]. So we have the
      equations

      {[
        s z : ('a list -> 'b) -> ('a -> 'b)
        p : (('c -> 'c) -> 'd) -> 'd
      ]}

      and we see ['c] = ['b] = ['a list], ['d] = ['a -> 'a list] and
      therefore, [p (s z)] has type ['a -> 'a list].

      Now for values,
      {[
         p (s z) = (s z) (fun x -> x)
                 = (fun k x -> z (fun v -> k (x :: v))) (fun x -> x)
                 = fun x -> z (fun v -> x :: v)
                 = fun x -> (fun k -> k []) (fun v -> x :: v)
                 = fun x -> (fun v -> x :: v) []
                 = fun x -> [x]
      ]}

      - What is the type of [p (s (s z))]?

       Given,
       {[
           s z : ('a list -> 'b) -> ('a -> 'b)
           s : (('a list -> 'b) -> 'c) -> ('a list -> 'b) -> 'a -> 'c
       ]}

       ['c] = ['a -> b] and [s (s z)] = [('a list -> 'b) -> 'a -> ('a -> 'b)]
       i.e.
       {[
           s (s z) :  ('a list -> 'b) -> ('a -> ('a -> 'b))
           p : (('c -> 'c) -> 'd) -> 'd
       ]}
    
       providing ['c] = ['b] = ['a list], ['d] = ('a -> ('a -> 'a list))
       and finally, [p (s s z)] has type ['a -> 'a -> 'a list].

       Here we go with computing the value:
       {[
          s z = fun k x -> z (fun v -> k (x :: v)) so,
          s (s z) = fun k' x' -> (s z) (fun v' -> k' (x' :: v'))
       ]}
       So,
       {[
          p (s (s z)) = 
             (fun k' x' -> (s z) (fun v' -> k' (x' :: v'))) (fun x -> x) =
             fun x' -> (s z) (fun v' -> x' :: v') =
             fun x' -> fun k x -> z (fun v -> k (x :: v))) (fun v' -> x' :: v') =
             fun x' -> fun x -> z (fun v -> (fun v' -> x' :: v') (x :: v)) =
             fun x' -> fun x -> (fun k -> k []) (fun v -> (fun v' -> x' :: v') (x :: v)) =
             fun x' -> fun x -> (fun v -> (fun v' -> x' :: v') (x :: v)) [] =
             fun x' -> fun x -> (fun v' -> x' :: v') [x] =
             fun x' -> fun x -> [x'; x]
       ]}

*)

(* This is an exercise in mapping over pairs, generic in the datatypes
   and number of arguments involved.

   [pair_map_1 f g (x, y) = (f x, g y)]
   [pair_map_2 f g (x, y) (x', y') = (f x x', g y y')]
   [pair_map_3 f g (x, y) (x', y') (x'', y'') = (f x x' x'', g y y' y'')]
      .
      .
      .
*)

let ( ** ) app k = fun x y -> k (app x y)
let pc k a b = k (a, b)
let papp (f1, f2) (x1, x2) = (f1 x1, f2 x2)
let pu x = x

(*The argument `f` in the below is for the sake of value restriction*)
let pair_map_1 f = pc (papp ** pu) (f : 'a -> 'b)
let pair_map_2 f = pc (papp ** papp ** pu) (f : 'a -> 'b -> 'c)
let pair_map_3 f = pc (papp ** papp ** papp ** pu) (f : 'a -> 'b -> 'c -> 'd)

(*Let's do the math. Start with [pair_map_1]

  [{
    First,

      pc (papp ** pu) =
      (\k f g. k (f, g)) (papp ** pu) =
      \f g. (papp ** pu) (f, g)

   Now,

     papp ** pu = \x y. pu (papp x y) =
     \x y. papp x y

  so,

    \f g. (papp ** pu) (f, g) =
    \f g. (\(a, b) (x, y). (a x, b y)) (f, g) =
    \f g. \(x, y). (f x, g y)
  that is,

    pair_map_1 = pc (papp ** pu) = \f g (x, y). (f x, g y).
  ]}

  The type can be read off from the above equation as
  [{
    val pair_map_1 = ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  }]

  On to [pair_map_2].
  [{
    pc (papp ** papp ** pu) =
    (\k f g. k (f, g)) (papp ** papp ** pu) =
    \f g. (papp ** papp ** pu) (f, g)
  }]

  where
  [{
    papp ** papp ** pu = papp ** (papp ** pu) =
    papp ** (\a' b'. pu (papp a' b')) =
    papp ** (\a' b'. papp a' b') = 
    \a b.(\a' b'. papp a' b') (papp a b)
  {]

  So, 
  [{
    pc (papp ** papp ** pu) = 
    \f g. (papp ** papp ** pu) (f, g) =
    \f g. (\a b.(\a' b'. papp a' b') (papp a b)) (f, g) =
    \f g. (\b. (\a' b'. papp a' b') (papp (f, g) b)) =
    \f g. \(x, y). \a' b'. (papp a' b') (papp (f, g) (x, y)) =
    \f g. \(x, y). \a' b'. (papp a' b') (f x, g y) =
    \f g. \(x, y). \b'. papp (f x, g y) b' =
    \f g. \(x, y). \(x', y'). papp (f x, g y) (x', y') =
    \f g. \(x, y). (x', y'). (f x x', g y y')
  }]

  that is, a function in two binary functions and two pairs e.g
  [{
   pair_map (+) (-) (1, 2) (3, 4) = (4, -2)
  }].

  Reading from the beta-normal form we conclude [pair_map_2] has type 
  [('a -> 'b -> 'c) -> ('d -> 'e -> 'f) -> ('a * 'd) -> ('b * 'e) -> ('c * 'f)]

*)
