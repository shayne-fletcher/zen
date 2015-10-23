//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fesum.exe /Zi /MDd /EHsc /I d:/boost_1_59_0 sum.cpp

#include "recursive_union.hpp"

#include <string>
#include <iostream>

namespace foo {

namespace detail {
  template <std::size_t I, class T, class... Ts>
  struct index_of_impl;

  template <std::size_t I, class T, class... Ts>
  struct index_of_impl<I, T, T, Ts...> {
    static auto const value = I;
  };

  template <std::size_t I, class T, class... Ts>
  struct index_of_impl<I, T, recursive_wrapper<T>, Ts...> {
    static auto const value = I;
  };

  template <std::size_t I, class X, class T, class... Ts> 
  struct index_of_impl<I, X, T, Ts...>{
    static auto const value = index_of_impl<I + 1, X, Ts...>::value;
  };

}//namespace<detail>

template <class T, class... Ts>
struct index_of {
  static auto const value = detail::index_of_impl<0u, T, Ts...>::value;
};

#if defined(_MSC_VER)
#  pragma warning(pop)
#endif//defined (_MSC_VER)

template <class... Ts>
class sum_type {
private:

  std::size_t cons;
  recursive_union<Ts...> data;
  
public:
  sum_type () = delete;

  sum_type (sum_type const& other) : cons (other.cons) {
    data.copy (cons, other.data);
  }

  sum_type (sum_type&& other) : cons (other.cons) {
    data.move (cons, std::move (other.data));
  }

  template <class T, class... Args>
  explicit sum_type (constructor<T> t, Args&&... args)
    : data (t, std::forward<Args>(args)...), cons (index_of<T, Ts...>::value)
  {}

  ~sum_type() {
    data.destruct (cons);
  }

  sum_type& operator= (sum_type const& other) {
    if (std::addressof (other) == this)
      return *this;

    data.destruct (const);
    cons = s.cons;
    data.copy (cons, s.data);

    return *this;
  }

};

}//namespace foo

struct E_const;
struct E_add;

using expression = foo::sum_type<E_const, foo::recursive_wrapper<E_add>>;

struct E_const { 
  int i; 
  E_const (int i) : i (i) {} 
};

struct E_add {
  expression l, r;
  E_add (expression const& l, expression const& r) : l (l), r (r) {}
};

int main () {

  expression xpr(
      foo::constructor<E_add>()
    , expression (foo::constructor<E_const>(), 2)
    , expression (foo::constructor<E_const>(), 3));

  return 0;
}
