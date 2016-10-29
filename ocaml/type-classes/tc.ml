(*Implementing and understanding type-classes
  http://okmij.org/ftp/Computation/typeclass.html*)

(*
  class Show a where
    show :: a -> string
*)

type 'a show = {
  show : 'a -> string
}

(*
  instance Show Bool where
    show True = "True"
    show False = "False"
*)

let show_bool : bool show = {
  show = function | true -> "True" | false -> "False"
}

(*
  instance Show Int where
    show = Prelude.show -- internal
*)

let show_int : int show = {
  show = string_of_int
}

(*
  print :: Show a -> a -> IO ()
  print x = putShowLn$ show x
*)

let print : 'a show -> 'a -> unit = 
  fun {show} -> fun x -> print_endline@@ show x

(* -- *)

(*
  class Num a where
    fromInt :: Int -> a
    (+)     :: a -> a -> a

  sum :: Num a => [a] -> a
  sum ls = foldr (+) (fromInt 0) ls

*)

type 'a num = {
  from_int : int -> 'a;
  add      : 'a -> 'a -> 'a;
}

let sum : 'a num -> 'a list -> 'a = 
  fun {from_int; add= ( + )} -> 
    fun ls ->
      List.fold_right ( + ) ls (from_int 0)

(*
  instance Num Int where
    fromInt x = x
    (+)       = (Prelude.+)
*)

let int_num  : int num  = { 
  from_int = (fun x -> x); 
  add      = Pervasives.( + ); 
}

let bool_num : bool num = {
  from_int = (function | 0 -> false | _ -> true);
  add = function | true -> fun _ -> true | false -> fun x -> x
}

(*
  print_incr :: (Show a, Num a) => a -> IO ()
  print_incr x = print$ x + fromInt 1
*)

let print_incr : ('a show * 'a num) -> 'a -> unit = 
  fun (show, {from_int; add= ( + )}) -> 
    fun x -> print show (x + (from_int 1))

(*
  print_incr_int :: Int -> IO ()
  print_incr_int x = print_incr x
*)

let print_incr_int : int -> unit = fun x ->
  print_incr (show_int, int_num) x

(*
  instance Show a => Show [a] where
    show xs = "[" ++ go True xs
      where
        go _ [] = "]"
        go first (h:t) =
          (if first then "" else ", ") ++ show h ++ go False t
*)

let show_list : 'a show -> 'a list show =
  fun {show} ->
    {show = fun xs ->
      let rec go first = function
        | [] -> "]"
        | h :: t ->
          (if (first) then "" else ", ") ^ show h ^ go false t in
      "[" ^ go true xs
    }

(* -- *)

(*

  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

  deriving instance Eq Bool
  deriving instance Eq Int

*)

type 'a eq = { 
  eq : 'a -> 'a -> bool; 
  neq : 'a -> 'a -> bool
}

let bool_eq : bool eq = {
  eq   = Pervasives. ( = );
  neq  = Pervasives. ( <> )
}

let int_eq : int eq = {
  eq   = Pervasives. ( = );
  neq  = Pervasives. ( <> )
}

(* 
  An example of a type-class with a super-class and a default

  class (Eq a, Num a) => Mul a where
    ( * ) :: a -> a -> a
    x * _ | x == fromInt 0 = fromInt 0
    x * y | x == fromInt 1 = y
    x * y = y + (x + (from Int (-1))) * y

  instance Mul Bool where
    -- default

  instance Mul Int where
    x * y = (Prelude. * ) x y -- internal

*)

type 'a mul = {
  mul_super : 'a eq * 'a num;
  mul : 'a -> 'a -> 'a
}

let mul_default : 'a eq * 'a num -> 'a mul = 
  fun (({eq}, {from_int; add = ( + )}) as super) -> 
    {
      mul_super = super;
      mul = let rec loop x y = begin match () with
      | () when eq x (from_int 0) -> from_int 0
      | () when eq x (from_int 1) -> y
      | () -> y + loop (x + (from_int (-1))) y 
      end in loop
    }

let bool_mul : bool mul = 
  mul_default (bool_eq, bool_num)

let int_mul : int mul = {
  mul_super = (int_eq, int_num);
  mul = Pervasives.( * )
}

(*
  dot :: Mul a => [a] -> [a] -> a
  dot xs ys = sum$ zipWith ( * ) xs ys
*)

let dot : 'a mul -> 'a list -> 'a list -> 'a = 
  fun {mul_super = (eq, num); mul} ->
    fun xs ys -> sum num@@ List.map2 mul xs ys

(* -- *)

(*
  print_nested :: Show a => Int -> a -> IO ()
  print_nested 0 x = print x
  print_nested n x = print_nested (n - 1) (replicate n x)

  test_nested = do
    n <- getLine
    print_nested (read n) (5::Int)
*)

let rec replicate : int -> 'a -> 'a list = 
  fun n x -> if n <= 0 then [] else x :: replicate (n - 1) x

(*Polymorphic recursion. The signature is mandatory here*)
let rec print_nested : 'a. 'a show -> int -> 'a -> unit =
  fun show_dict -> function
  | 0 -> fun x -> print show_dict x
  | n -> fun x -> print_nested (show_list show_dict) (n - 1) (replicate n x)
(*At first blush, the code is straightforward. After seeing the output
  one realizes the printed value , the deeply nested list
  `[[[Int]...]]` is not statically known. It depends on the value of
  [n] received from the user at run-time. Since we don't know the
  exact type of [x] at compile time, we can't statically build
  evidence that it is showable. The compiler must arrange for building
  such evidence dynamically. The code illustrates such an arrangement
  : as we add one more `list` to the type, we transform the current
  [show_dict] with one more [show_list]*)

let test_nested =
  let n = read_int () in
  print_nested show_int n 5

(*If entered 4

[[[[5, 5, 5, 5], [5, 5, 5, 5], [5, 5, 5, 5]], [[5, 5, 5, 5], [5, 5, 5, 5], [5, 5, 5, 5]]]]

*)

(* -- *)

type _ trepr = 
| Int : int trepr
| Bool : bool trepr

let show : type a. a trepr -> a -> string  = fun _ x -> 
  failwith "failed overload resolution"

let show : type a. a trepr -> a -> string = function
  | Bool -> string_of_bool
  | t -> show t

let show : type a. a trepr -> a -> string = function
  | Int -> string_of_int
  | t -> show t

let test_show : string = show Bool true

let print : 'a trepr -> 'a -> unit = 
  fun trepr x -> print_endline (show trepr x)
