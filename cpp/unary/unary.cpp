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

num from_int (int i) {
  if (i == 0){
    return Z {};
  }
  else{
    return S {from_int (i - 1)};
  }
}

num add (num l, num r);

struct add_visitor : boost::static_visitor<num> {
  num operator () (Z, S s) const { return s; }
  num operator () (S s, Z) const { return s; }
  num operator () (Z, Z) const { return Z {}; }
  num operator () (S s, S t) const { return S { add (s.i, t) }; }
};

num add (num l, num r) {
  return boost::apply_visitor (add_visitor (), l, r);
}

num succ (num x) { return S{x}; }

struct prd_visitor : boost::static_visitor<num>{
  num operator () (Z z) const { return z; }
  num operator () (S s) const { return s.i; }
};

num prd (num x) {
  return boost::apply_visitor(prd_visitor (), x);
}

num sub (num x, num y);

struct sub_visitor : boost::static_visitor<num> {
  num operator () (Z, Z) const { return Z {}; }
  num operator () (Z, S) const { return Z {}; }
  num operator () (S m, Z) const { return m; }
  num operator () (S m, S n) const { return sub (m.i, prd (n)); }
};

num sub (num x, num y) { return boost::apply_visitor (sub_visitor (), x, y); }

int main () {

  num zero = Z {};
  num one = succ (zero);
  num two = succ (succ (zero));
  num three = succ (succ (succ (zero)));

  std::cout << to_int (add (two, three)) << std::endl;

  std::cout << to_int (sub (from_int (23), from_int (12))) << std::endl;

  return 0;
}
