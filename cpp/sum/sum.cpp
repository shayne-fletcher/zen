//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fesum.exe /Zi /MDd /EHsc /I d:/boost_1_59_0 sum.cpp


#include "or.hpp"
#include "sum.hpp"

#include <string>
#include <iostream>

/*
struct E_const;
struct E_add;

using expression = 
  foo::sum_type<E_const, foo::recursive_wrapper<E_add>>;

struct E_const { 
  int i; 
  E_const (int i) : i (i) {} 
};

struct E_add {
  expression l, r;
  E_add (expression const& l, expression const& r) : l (l), r (r) {}
};

std::ostream& operator << (std::ostream& os, expression const& e) {
  e.match (
    [&](E_const e) { os << e.i;  },
    [&](E_add e) { os << e.l << "+" << e.r;  }
  );

  return os;
}

int main () {

  expression xpr{
    foo::constructor<E_add>{}
      , expression {foo::constructor<E_const>(), 2}
      , expression {foo::constructor<E_const>(), 3}};

  std::cout << xpr << std::endl;

  return 0;
}
*/


struct Foo {};
struct Bar {};

using variant_t = 
  foo::sum_type<foo::recursive_wrapper<Bar>, Foo>;

std::ostream& operator << (std::ostream& os, variant_t const& v) {
  v.match (
        [&](Bar) { os << "Bar"; }
     ,  [&](Foo const&) { os << "Foo"; }
  );

  return os;
}

int main () {
   
  variant_t foobar {foo::constructor<Bar>()};

  std::cout << foobar << std::endl;

  return 0;
}

