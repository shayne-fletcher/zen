/*
type num = Z | S of num

let rec add (x : num) (y : num) : num =
  match (x, y) with 
  | (Z, _) -> y
  | (_, Z) -> x
  | (S m, n) -> S (add m n)

let rec mul (x : num) (y : num) : num = 
  match (x, y) with
  | (Z, _) -> Z
  | (_, Z) -> Z
  | (S Z, x) -> x
  | (x, S Z) -> x
  | (S m, n) -> add (mul m n) n
*/

//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl /Feunary.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 unary.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>

#include <stdexcept>
#include <iostream>

struct Z;
struct S;

typedef boost::variant<Z, boost::recursive_wrapper<S>> num;

struct Z {};
struct S { num i; };

int to_int (num const& i);

struct to_int_visitor 
  : boost::static_visitor<int> {
  int operator ()(Z const& n) const { return 0; }
  int operator ()(S const& n) const { return 1 + to_int (n.i); }
};

int to_int (num const& i) {
  return boost::apply_visitor (to_int_visitor (), i);
}

num add (num l, num r);

struct add_visitor : boost::static_visitor<num> {
  num operator () (Z, S s) const { return s; }
  num operator () (S s, Z) const { return s; }
  num operator () (S s, S t) const { return S{add (s.i, t)}; }

  template <class U, class V> 
    num operator ()(U, V) const { throw std::runtime_error("bah"); }
};

num add (num l, num r) {
  return boost::apply_visitor (add_visitor (), l, r);
}

template <class T> num succ (T const& x) { return S{x}; }

int main () {

  num zero = Z{};
  num one = succ (zero);
  num two = succ (succ (zero));
  num three = succ (succ (succ (zero)));

  std::cout << to_int (add (two, three)) << std::endl;

  return 0;
}
