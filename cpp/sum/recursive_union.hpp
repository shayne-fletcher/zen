#if !defined(RECURSIVE_UNION_CCA42501_F1D9_4A56_98DB_8D590809BB14_H)
# define RECURSIVE_UNION_CCA42501_F1D9_4A56_98DB_8D590809BB14_H

#include "and.hpp"
#include "recursive_wrapper.hpp"
#include "constructor.hpp"
#include "range.hpp"
#include "is_callable.hpp"

#include <cstddef>
#include <stdexcept>
#include <type_traits>

namespace foo {

template <class ...>
struct recursive_union {}; //fwd. decl.

//union_visitor  

template <class T>
struct overload_tag {};

template <class R, class T, class...>
struct union_visitor;

//Result type 'R'
template <class R, class T, class... Ts>
struct union_visitor<R, T, Ts...> {

  using result_type = R;

  //For 'F' callable with 'T const&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<is_callable<F, T>::value>::type
    >
  static result_type visit (overload_tag<O>, T const& t, F&& f, Fs&&...) {
    return std::forward<F>(f)(t);
  }

  //For 'F' not callable with 'T const&'
  template <
    class F, class O, class... Fs,
    class = typename std::enable_if<!is_callable<F, T>::value>::type
    >
  static result_type visit (overload_tag<O> o, T const& t, F&&, Fs&&... fs) {
    return union_visitor::visit (o, t, std::forward<Fs>(fs)...);
  }

  //For 'F' callable with 'T&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<is_callable<F, T>::value>::type
    >
  static result_type visit (overload_tag<O>, T& t, F&& f, Fs&&...) {
    return std::forward<F>(f)(t);
  }

  //For 'F' not callable with 'T&'
  template <
    class F, class O, class... Fs,
    class = typename std::enable_if<!is_callable<F, T>::value>::type
    >
  static result_type visit (overload_tag<O> o, T& t, F&&, Fs&&...fs) {
    return union_visitor::visit (o, t, std::forward<Fs>(fs)...);
  }
};

//Result type 'void'
template <class T, class... Ts>
struct union_visitor<void, T, Ts...> {

  using result_type = void;//result type of 'visit'

  //For 'F' callable with 'T const&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<is_callable<F, T>::value>::type
    >
  static result_type visit (overload_tag<O>, T const& t, F&& f, Fs&&...) {
    std::forward<F>(f)(t);
  }

  //For 'F' not callable with 'T const&'
  template <
    class F, class O, class... Fs,
    class = typename std::enable_if<!is_callable<F, T>::value>::type
    >
  static result_type visit (overload_tag<O> o, T const& t, F&&, Fs&&... fs) {
    union_visitor::visit (o, t, std::forward<Fs>(fs)...);
  }

  //For 'F' callable with 'T&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<is_callable<F, T>::value>::type
    >
  static result_type visit (overload_tag<O>, T& t, F&& f, Fs&&...) {
    std::forward<F>(f)(t);
  }

  //For 'F' not callable with 'T&'
  template <
    class F, class O, class... Fs,
    class = typename std::enable_if<!is_callable<F, T>::value>::type
    >
  static result_type visit (overload_tag<O> o, T& t, F&&, Fs&&...fs) {
    union_visitor::visit (o, t, std::forward<Fs>(fs)...);
  }
};

//For invalid index
struct invalid_sum_type_access : std::logic_error {
  explicit invalid_sum_type_access (std::string const& what)
    : logic_error (what)
  {}
  explicit invalid_sum_type_access (std::string&& what)
    : logic_error (std::move (what))
  {}
  explicit invalid_sum_type_access (char const* what)
    : logic_error (what)
  {}
};

//'Ts' return type 'R'
template <class R, class... Ts>
struct union_visitor<R, range<>, Ts...> {

  using result_type = R;//result type of 'visit'

  template <class... Fs>
  static result_type visit (recursive_union<Ts...> const&, std::size_t, Fs&&...) {
    throw invalid_sum_type_access{""};
  }

  template <class... Fs>
  static result_type visit (recursive_union<Ts...>&, std::size_t, Fs&&...) {
    throw invalid_sum_type_access{""};
  }
};

//'Ts' return type void
template <class... Ts>
struct union_visitor<void, range<>, Ts...> {

  using result_type = void;//the result 'visit'

  template <class... Fs>
  static result_type visit (recursive_union<Ts...> const&, std::size_t, Fs&&...) {
    throw invalid_sum_type_access{""};
  }

  template <class... Fs>
  static result_type visit (recursive_union<Ts...>&, std::size_t, Fs&&...) {
    throw invalid_sum_type_access{""};
  }
};

//'T', 'Ts', return type 'R'
template <class R, std::size_t I, std::size_t... Is, class T, class... Ts>
struct union_visitor<R, range<I, Is...>, T, Ts...> {

  using type = T; //the type of the value
  using result_type = R; //result type of 'visit'

  //'const' overload ('recursive_union<type, Ts...> const& u')
  template <class... Fs>
  static result_type visit (
    recursive_union<type, Ts...> const& u, std::size_t i, Fs&&... fs) {
    if (i == I) {
      //'u' is not a reference (no call 'get ()')
      overload_tag<type> o{};
      return union_visitor<result_type, T>::visit(o, u.v, std::forward<Fs>(fs)...);
    }
    else {
      return union_visitor<result_type, range<Is...>, Ts...>::visit(u.r, i, std::forward<Fs>(fs)...);
    }
  }

  //'non-const' overload ('recursive_union<T, Ts...>& u')
  template <class... Fs>
  static result_type visit (
    recursive_union<T, Ts...>& u, std::size_t i, Fs&&... fs) {
    if (i == I) {
      //'u' is not a reference (no call 'get ()')
      overload_tag<T> o{};
      return union_visitor<result_type, T>::visit(o, u.v, std::forward<Fs>(fs)...);
    }
    else {
      return union_visitor<result_type, range<Is...>, Ts...>::visit(u.r, i, std::forward<Fs>(fs)...);
    }
  }
};

//U='recursive_wrapper<T>', 'Ts', return type 'R'
template <class R, std::size_t I, std::size_t... Is, class T, class... Ts>
struct union_visitor<R, range<I, Is...>, recursive_wrapper<T>, Ts...> {

  using type = T; //the type held by the value
  using U = recursive_wrapper<type>;//the type of the value
  using result_type = R;//the type returned by 'visit'
  
  //'const' overload ('recursive_union<recursive_wrapper<type>, Ts...> const& u')
  template <class... Fs>
  static result_type visit (
     recursive_union<U, Ts...> const& u, std::size_t i, Fs&&... fs) {
    if (i == I) {
      //'u' is of type recursive_wrapper<type>, call 'get ()'
      overload_tag<type> o{};
      return union_visitor<result_type, type>::visit(o, u.v.get (), std::forward<Fs>(fs)...);
    }
    else {
      return union_visitor<result_type, range<Is...>, Ts...>::visit(u.r, i, std::forward<Fs>(fs)...);
    }
  }

  //'non-const' overload ('recursive_union<U, Ts...>& u')
  template <class... Fs>
  static result_type visit (
    recursive_union<U, Ts...>& u, std::size_t i, Fs&&... fs) {
    if (i == I) {
      //'u' is of type recursive_wrapper<type>, call 'get ()'
      overload_tag<type> o{};
      return union_visitor<result_type, T>::visit(o, u.v.get (), std::forward<Fs>(fs)...);
    }
    else {
      return union_visitor<result_type, range<Is...>, Ts...>::visit(u.r, i, std::forward<Fs>(fs)...);
    }
  }
};

//'T', 'Ts', return type 'void'
template <std::size_t I, std::size_t... Is, class T, class... Ts>
struct union_visitor<void, range<I, Is...>, T, Ts...> {

  using type = T; //the type of the value
  using result_type = void; //result type of 'visit'

  //'const' overload ('recursive_union<type, Ts...> const& u')
  template <class... Fs>
  static result_type
  visit (
    recursive_union<type, Ts...> const& u, std::size_t i, Fs&&... fs) {
    if (i == I) {
      //'u' is not a reference (no call 'get ()')
      overload_tag<type> o{};
      union_visitor<result_type, type>::visit(o, u.v, std::forward<Fs>(fs)...);
    }
    else {
      union_visitor<result_type, range<Is...>, Ts...>::visit(u.r, i, std::forward<Fs>(fs)...);
    }
  }

  //'non-const' overload ('recursive_union<T, Ts...>& u')
  template <class... Fs>
  static result_type
  visit (
    recursive_union<T, Ts...>& u, std::size_t i, Fs&&... fs) {
    if (i == I) {
      //'u' is not a reference (no call 'get ()')
      overload_tag<type> o{};
      union_visitor<result_type, type>::visit(o, u.v, std::forward<Fs>(fs)...);
    }
    else {
      union_visitor<result_type, range<Is...>, Ts...>::visit(u.r, i, std::forward<Fs>(fs)...);
    }
  }

};

//U='recursive_wrapper<T>', 'Ts', return type 'void'
template <std::size_t I, std::size_t... Is, class T, class... Ts>
struct union_visitor<void, range<I, Is...>, recursive_wrapper<T>, Ts...> {

  using type = T; //the type held by the value
  using U = recursive_wrapper<type>;//the type of the value
  using result_type = void;//the type returned by 'visit'

  //'const' overload ('recursive_union<recursive_wrapper<type>, Ts...> const& u')
  template <class... Fs>
  static result_type visit (
    recursive_union<U, Ts...> const& u, std::size_t i, Fs&&... fs) {
    if (i == I) {

      //'u' is of type recursive_wrapper<type>, call 'get ()'
      overload_tag<type> o{};
      union_visitor<void, type>::visit(o, u.v.get (), std::forward<Fs>(fs)...);

    }
    else {
      union_visitor<void, range<Is...>, Ts...>::visit (u.r, i, std::forward<Fs>(fs)...);
    }
  }

  //'non-const' overload ('recursive_union<U, Ts...>& u')
  template <class... Fs>
  static result_type visit (
    recursive_union<U, Ts...>& u, std::size_t i, Fs&&... fs) {
    if (i == I) {
      //'u' is of type recursive_wrapper<type>, call 'get ()'
      overload_tag<type> o{};
      union_visitor<void, type>::visit(o, u.v.get (), std::forward<Fs>(fs)...);
    }
    else {
      union_visitor<void, range<Is...>, Ts...>::visit(u.r, i, std::forward<Fs>(fs)...);
    }
  }
};

//--


//recursive_union

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
    , std::is_same<U, unwrap_recursive_t<T>>>::value, int>::type = 0
  >
  explicit recursive_union (constructor<U>, Args&&... args)
    noexcept (std::is_nothrow_constructible<U, Args...>::value)
  : v (std::forward<Args>(args)...)
  {}

  //'U' is not 'T' and 'T' is not a recursive wrapper or, 'U' is not
  //the type contained in 'T'.
  template <class U, class... Args,
  typename std::enable_if<
    !and_<
      is_recursive_wrapper<T>
    , std::is_same<U, unwrap_recursive_t<T>>>::value, int>::type = 0
  >
  explicit recursive_union (constructor<U> t, Args&&... args)
    noexcept(std::is_nothrow_constructible<Ts..., constructor<U>, Args...>::value)
    : r (t, std::forward<Args>(args)...)
  {}

  ~recursive_union () 
  {}

  void copy (std::size_t i, recursive_union const& u)
    noexcept(
     std::is_nothrow_copy_constructible<T>::value
     && noexcept (std::declval<recursive_union>().r.copy (i - 1, u.r))
    ) {
    if (i == 0) {
      new (std::addressof (v)) T (u.v);
    }
    else {
      r.copy (i - 1, u.r);
    }
  }

  void move (std::size_t i, recursive_union const& u) 
    noexcept (
      std::is_nothrow_move_constructible<T>::value
     && noexcept (std::declval<recurive_union>().r.move (i - 1, std::move (u.r)))
    ) {
    if (i == 0) {
      new (std::addressof (v)) T (std::move (u.v));
    }
    else {
      r.move (i - 1, std::move (u.r));
    }
  }

  void destruct (std::size_t i)
    noexcept (std::is_nothrow_destructible<T>::value 
     && noexcept (std::declval<recursive_union>().r.destruct (i - 1))) {
    if (i == 0) {
      v.~T ();
    }
    else {
      r.destruct (i - 1);
    }
  }

  bool compare (size_t i, recursive_union const& rhs) const 
    noexcept {
    return i == 0 ? v == rhs.v : r.compare (i - 1, rhs.r);
  }

  union {
    T v;
    recursive_union<Ts...> r;
  };
};

}//namespace foo

#endif //!defined(RECURSIVE_UNION_CCA42501_F1D9_4A56_98DB_8D590809BB14_H)
