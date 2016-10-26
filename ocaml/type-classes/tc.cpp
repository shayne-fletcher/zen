//Implementing Haskell type-classes in C++

#include <string>
#include <iostream>
#include <vector>
#include <iterator>

/*
  class Show a where
    show :: a -> string
 */

template <class A> struct Show {};

/*
  instance Show Int where
    show x = Prelude.show x -- internal
 */

template <>
struct Show<int> {
  static std::string (*show)(int);
};
std::string(*Show<int>::show)(int) = &std::to_string;

/*
  instance Show Bool where
    str True = "True"
    str False = "False"
*/

template <>
struct Show<bool> {
  static std::string show (bool);
};
std::string Show<bool>::show (bool b) { return b ? "true" : "false"; }

/*
  -- Parameterically overloaded function

  print :: Show a => a -> IO ()
  print x = putShowLn$ str x
*/

//No evidence of the argument's membership is required, the compiler
//synthesizes it

template <class A>
void print (A const& a) {
  std::cout << Show<A>::show (a) << std::endl;
}

/*
  -- Instantation

  test_print :: IO ()
  test_print = print True
*/

auto test_print = []() { print (true); };

// --

/*
  class Num a where
    fromInt :: Int -> a
    (+)     :: a -> a -> a

  sum :: Num a => [a] -> a
  sum ls = foldr (+) (fromInt 0) ls

*/

template <class A>
struct Num {};

namespace detail {
  template <class F, class A, class ItT>
  A fold_right (F f, A z, ItT begin, ItT end) {
    if (begin == end) return z;
    return f (fold_right (f, z, std::next (begin), end), *begin);
  }
}//namespace<detail> 

template <class ItT>
typename std::iterator_traits<ItT>::value_type 
sum (ItT begin, ItT end) {
  using A = std::iterator_traits<ItT>::value_type;
  auto add = Num<A>::add;
  auto from_int = Num<A>::from_int;

  return detail::fold_right (
    [=](A z, auto x) { return add (x, z); }, from_int (0), begin, end);

  return 0;
}

/*
  -- Sample instance

  instance Num Int where
    fromInt x = x
    (+)       = (Prelude.+)
*/

template <>
struct Num<int> {
  static int from_int (int);
  static int add (int, int);
};

int Num<int>::from_int (int i) { return i; }
int Num<int>::add (int x, int y) { return x + y; }

template <>
struct Num<bool> {
  static bool from_int (int);
  static bool add (bool, bool);
};

bool Num<bool>::from_int (int i) { return i != 0; }
bool Num<bool>::add (bool x, bool y) { if (x) return true; return y; }

/*
  -- Two constraints

  print_incr :: (Show a, Num a) => a -> IO ()
  print_incr x = print$ x + fromInt 1
*/

template <class A>
void print_incr (A x) {
  print (Num<A>::add (x, Num<A>::from_int (1)));
}

/*
  -- Instantation of the above

  print_incr_int :: Int -> IO
  print_incr_int x = print_incr x
*/

auto print_incr_int = [](int x) { return print_incr (x); };

// --

/*
  -- An instance with a constraint

  instance Show a => Show [a]   where
    show xs = "[" ++ go True xs
      where
        go _ [] = "]"
        go first (h:t) =
         (if first then "" else ", ")  ++ show h ++ go False t

   testls :: String
   testls = show [1::int, 2, 3]
*/

//It doesn't look like we can do much better than this (a partial
//specialization for each possible sequence type). This can't be
//interpreted as saying "for all types `A`, `vector<A>` is
//showable. Read on.
template <class A>
struct Show<std::vector<A>> {
  static std::string show (std::vector<A> const& ls);
};

//If `A` is showable, this will compile. If it's not it won't.
template <class A>
std::string Show<std::vector<A>>::show (std::vector<A> const& ls) {
  bool first=true;
  std::vector<A>::const_iterator begin=ls.begin (), end=ls.end ();
  std::string s="[";
  while (begin != end) {
    if (first) first = false;
    else s += ", ";
    //A compile time error will result here if if there is no
    //evidence that `A` is in `Show`
    s += Show<A>::show (*begin++);
  }
  s += "]";

  return s;
}

/* -- */

/*
  class Eq where
    (==) :: a -> a -> bool
    (/=) :: a -> a -> bool

  deriving instance Eq Bool
  deriving instance Eq Int
*/

template <class A> struct Eq {};

template <>
struct Eq<bool> {
  static bool eq (bool, bool);
  static bool neq (bool, bool);
};

bool Eq<bool>::eq (bool s, bool t) { return s == t; }
bool Eq<bool>::neq (bool s, bool t) { return s != t; }

template <>
struct Eq<int> {
  static int eq (int, int);
  static int neq (int, int);
};

int Eq<int>::eq (int s, int t) { return s == t; }
int Eq<int>::neq (int s, int t) { return s != t; }

/*
  -- Type-class with a super-classes and a default method

  class (Eq a, Num a) => Mul a where
    (*) :: a -> a -> a
    x * _ | x == fromInt 0 = fromInt 0
    x * y | x == fromInt 1 = y
    x * y | y + (x + (fromInt (-1))) * y
*/

template <class A>
struct Mul : Eq<A>, Num <A>{
  static A mul (A x, A y);
};

template <class A>
A Mul<A>::mul (A x, A y) {
  if (eq (x, from_int (0))) return from_int (0);
  if (eq (x, from_int (1))) return y;

  return add (y, mul ((add (x, from_int (-1))), y));
}

/*
  instance Mul Bool where
    -- default

  instance Mul Int where
    x * y = (Prelude.*) x y -- internal

*/

template Mul<bool>;
template <> int Mul<int>::mul (int x, int y) { return x * y; }

/*
  -- Dot product. There is only one constraint

  dot :: Mul a -> [a] -> [a] -> a
  dot xs ys = sum$ zipWith (*) xs ys
*/

namespace detail{

  template <class F, class It, class Acc>
  Acc map2 (F f, It xs_begin, It xs_end, It ys_begin, It ys_end, Acc acc) {
    if ((xs_begin == xs_end) || (ys_begin == ys_end)) return acc;
    return map2 (f
          , std::next (xs_begin)
          , xs_end
          , std::next (ys_begin)
          , ys_end
          , *acc++ = f (*xs_begin, *ys_begin));
  }

}//namespace detail

template <class A>
A dot (std::vector<A> const& xs, std::vector<A> const& ys) {
  std::vector<A> buf;
  detail::map2 (
     Mul<A>::mul
   , xs.begin (), xs.end()
   , ys.begin (), ys.end ()
   , std::back_inserter(buf));
  return sum (buf.begin (), buf.end ());
}

/*
  test_dot :: Int
  test_dot = dot [1, 2, 3] [4, 5, 6]
*/

int test_dot = dot (std::vector<int>{1, 2, 3}, std::vector<int>{4, 5, 6});

/*
  -- Polymorphic recursion

  print_nested :: Show a => Int -> a -> IO ()
  print_nested 0 x = print x
  print_nested n x = print_nested (n - 1) (replicate n x)

  test_nested = do
    n <- getLine
    print_nested (read n) (5::Int)

 */

#if 0

namespace detail {

  template<class A, class ItT>
  ItT replicate (int n, A x, ItT dst) {

    if (n <= 0) return dst;
    return replicate ((n - 1), x, *dst++ = x);
  }

}//namespace detail


//This code sends the compiler (msvc-14.0) into unbounded recursion!

template <class A>
void print_nested (int n, A const& x) {
  if (n == 0)
    print (x);
  else {
    std::vector<A> buf;
    detail::replicate(n, x, std::back_inserter(buf));
    print_nested (n - 1, buf);
  }
}

void test_nested () {
  int n;
  std::cin >> n;
  print_nested (n, 5);
}

#endif//if 0

//Test

int main () {
  
  return 0;
}
