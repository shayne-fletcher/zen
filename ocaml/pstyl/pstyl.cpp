// This is an exercise in mapping over pairs, generic in the datatypes
// and number of arguments involved.
//
//    [pair_map_1 f g (x, y) = (f x, g y)]
//    [pair_map_2 f g (x, y) (x', y') = (f x x', g y y')]
//    [pair_map_3 f g (x, y) (x', y') (x'', y'') = (f x x' x'', g y y' y'')]
//       .
//       .
//       .
//See Oleg Kiselyov's notes at
//http://okmij.org/ftp/Computation/extra-polymorphism.html.

#include <utility>
#include <iostream>

//let pu x = x
auto pu = [](auto x) { return x; };

//let ( ** ) app k  = fun x y -> k (app x y)
template <class F, class K>
auto operator ^ (F app, K k) {
  return [=](auto x) {
    return [=] (auto y) {
      return k ((app (x)) (y));
    };
  };
}

//let pc k a b = k (a, b)
auto pc = [](auto k) {
  return [=](auto a) {
    return [=](auto b) { 
      return k (std::make_pair (a, b)); };
  };
};

//let papp (f, g) (x, y) = (f x, g y)
auto papp = [](auto f) { 
  return [=](auto x) { 
    return std::make_pair (f.first (x.first), f.second (x.second)); };
};

int main () {

  auto pair = &std::make_pair<int, int>;

  {
  //Given `f`, `g` functions in one argument and point `(x, y)`,
  //compute `(f x, g y)`
  auto succ= [](int x){ return x + 1; };
  auto pred= [](int x){ return x - 1; };
  auto p  = (pc (papp ^ pu)) (succ) (pred) (pair (1, 2));
  std::cout << p.first << ", " << p.second << std::endl;
  }

  {
  //Given `f`, `g` functions in two arguments and points `(x, y), (x',
  //y')`, compute `(f x x', g y y')`
  auto add = [](int x) { return [=](int y) { return x + y; }; };
  auto sub = [](int x) { return [=](int y) { return x - y; }; };
  auto p = pc (papp ^ papp ^ pu) (add) (sub) (pair(1, 2)) (pair (3, 4));
  std::cout << p.first << ", " << p.second << std::endl;
  }

  return 0;
}
