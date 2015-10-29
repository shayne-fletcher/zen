//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fesum.exe /Zi /MDd /EHsc /I d:/boost_1_59_0 sum.cpp

//#include "or.hpp"
#include "sum.hpp"

#include <string>
#include <iostream>

struct E_const;
struct E_add;
struct E_sub;
struct E_sub;
struct E_mul;
struct E_div;

using xpr_t = foo::sum_type<
    E_const
  , foo::recursive_wrapper<E_add>
  , foo::recursive_wrapper<E_sub>
  , foo::recursive_wrapper<E_mul>
  , foo::recursive_wrapper<E_div>
  >;

struct E_const { 
  int i; 
  E_const (int i) : i (i) 
  {} 
};
struct E_add {
  xpr_t l, r;
  E_add (xpr_t const& l, xpr_t const& r) : l (l), r (r) 
  {}
};
struct E_sub {
  xpr_t l, r;
  E_sub (xpr_t const& l, xpr_t const& r) : l (l), r (r) 
  {}
};
struct E_mul {
  xpr_t l, r;
  E_mul (xpr_t const& l, xpr_t const& r) : l (l), r (r) 
  {}
};
struct E_div {
  xpr_t l, r;
  E_div (xpr_t const& l, xpr_t const& r) : l (l), r (r) 
  {}
};

std::ostream& operator << (std::ostream& os, xpr_t const& e) {

  return e.match<std::ostream&> (

    [&](E_const const& e) -> auto& { return os << e.i;  },
    [&](E_mul const& e) -> auto& { return os << e.l << "*" << e.r;  },
    [&](E_div const& e)-> auto&  { return os << e.l << "/" << e.r;  },
    [&](E_add const& e) -> auto& { return os << e.l << " + " << e.r;  },
    [&](E_sub const&e)-> auto& { return os << e.l << " - " << e.r;  }
  );

  //  return os;
}

int main () {

  //n=2 + 3
  xpr_t n{
    foo::constructor<E_add>{}
    , xpr_t {foo::constructor<E_const>(), 2}
    , xpr_t {foo::constructor<E_const>(), 3}};
  //d=5
  xpr_t d{foo::constructor<E_const>{}, 5};
  //xpr = 2 + 3/5
  xpr_t xpr = xpr_t{foo::constructor<E_div>{}, n, d};

  std::cout << xpr << std::endl;

  return 0;
}

/*
//Breathing test

struct Foo {};
struct Bar {};
struct Baz {};

using variant_t = 
  foo::sum_type<
     foo::recursive_wrapper<Bar>
   , foo::recursive_wrapper<Foo>
   , Baz
>;

std::ostream& operator << (std::ostream& os, variant_t const& v) {
  v.match (
        [&](Bar const&) { os << "Bar"; }
     ,  [&](Foo const&) { os << "Foo"; }
     ,  [&](Baz const&) { os << "Baz"; }
  );

  return os;
}

int main () {
   
  std::cout << variant_t{foo::constructor<Foo>{}} << std::endl;
  std::cout << variant_t{foo::constructor<Bar>{}} << std::endl;
  std::cout << variant_t{foo::constructor<Baz>{}} << std::endl;

  return 0;
}
*/
