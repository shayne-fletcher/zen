//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Feu.exe /Zi /MDd /EHsc /I d:/boost_1_59_0 u.cpp

#include "and.hpp"
#include "recursive_wrapper.hpp"

#include <type_traits>
#include <cstddef>
#include <string>
#include <iostream>

namespace foo {

template <std::size_t I, class T, class... Ts>
struct index_of_impl;

template <std::size_t I, class T, class... Ts>
struct index_of_impl<I, T, T, Ts...> {
  static std::size_t const value = I;
};

template <std::size_t I, class T, class... Ts>
struct index_of_impl<I, T, recursive_wrapper<T>, Ts...> {
  static std::size_t const value = I;
};

template <std::size_t I, class X, class T, class... Ts> 
struct index_of_impl<I, X, T, Ts...>{
  static std::size_t const value = index_of_impl<I + 1, X, Ts...>::value;
};

template <class T, class... Ts>
struct index_of {
  static std::size_t const value = index_of_impl<0u, T, Ts...>::value;
};

template <class>
struct constructor {};

template <class ...>
struct recursive_union {};

template <>
struct recursive_union<> {
  void copy (std::size_t, recursive_union const&) {}
  void move (std::size_t, recursive_union&&) {}
  void destruct (std::size_t) {}
  bool compare (std::size_t, recursive_union const&) const { return false; }
};

#if defined(_MSC_VER)
#  pragma warning(push)
#  pragma warning(disable:4624)
#endif//defined (_MSC_VER)

template <class T, class... Ts>
struct recursive_union<T, Ts...> {

  recursive_union () 
  {}

  //Match on 'T'
  template <class... Args>
  explicit recursive_union (constructor<T>, Args&&... args) 
    : v (std::forward<Args>(args)...)
  {}

  //'U' is not 'T' but 'T' is a recursive wrapper and 'U' is the type
  //contained in 'T'
  template <class U, class... Args,
  typename std::enable_if<
     and_<
      is_recursive_wrapper<T>
    , std::is_same<U, typename unwrap_recursive<T>::type>>::value, int>::type = 0
  >
  explicit recursive_union (constructor<U>, Args&&... args)
  : v (std::forward<Args>(args)...)
  {}

  //'U' is not 'T' and 'T' is not a recursive wrapper or, 'U' is not
  //the type contained in 'T'.
  template <class U, class... Args,
  typename std::enable_if<
    !and_<
      is_recursive_wrapper<T>
    , std::is_same<U, typename unwrap_recursive<T>::type>>::value, int>::type = 0
  >
  explicit recursive_union (constructor<U> t, Args&&... args)
    : r (t, std::forward<Args>(args)...)
  {}

  ~recursive_union () 
  {}

  void copy (std::size_t i, recursive_union const& u) {
    if (i == 0) {
      new (std::addressof (v)) T (u.v);
    }
    else {
      r.copy (i - 1, u.r);
    }
  }

  void move (std::size_t i, recursive_union const& u) {
    if (i == 0) {
      new (std::addressof (v)) T (std::move (u.v));
    }
    else {
      r.move (i - 1, std::move (u.r));
    }
  }

  void destruct (std::size_t i) {
    if (i == 0) {
      v.~T ();
    }
    else {
      r.destruct (i - 1);
    }
  }

  bool compare (size_t i, recursive_union const& rhs) const {
    return i == 0 ? v == rhs.v : r.compare (i - 1, rhs.r);
  }

  union {
    T v;
    recursive_union<Ts...> r;
  };
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
