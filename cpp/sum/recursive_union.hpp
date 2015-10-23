#if !defined(RECURSIVE_UNION_CCA42501_F1D9_4A56_98DB_8D590809BB14_H)
# define RECURSIVE_UNION_CCA42501_F1D9_4A56_98DB_8D590809BB14_H

#include "and.hpp"
#include "recursive_wrapper.hpp"
#include "constructor.hpp"

#include <cstddef>
#include <type_traits>

namespace foo {

//union_indexer

template <class ...>
struct recursive_union {}; //fwd. decl.

/*
  let rec drop l i =
    if i = 0 then l else
    match l with
     | [] -> []
     | (h :: tl) -> drop tl (i - 1)
*/

template <std::size_t I, class T, class... Ts>
struct union_indexer {

  static auto ref (recursive_union<T, Ts...>& u) 
    /*-> decltype (union_indexer<I - 1, Ts...>::ref (u.r))*/ {
    return union_indexer<I - 1, Ts...>::ref (u.r)
  }

  static auto ref (recursive_union<T, Ts...> const& u) 
    /*-> decltype (union_indexer<I - 1, Ts...>::ref (u.r))*/ {
    return union_indexer<I - 1, Ts...>::ref (u.r)
  }

  static auto ptr (recursive_union<T, Ts...> const& u) 
    /*-> decltype (union_indexer<I - 1, Ts...>::ptr (u.r))*/ {
    return union_indexer<I - 1, Ts...>::ptr (u.r)
  }

};

template <class T, class... Ts>
struct union_indexer<0, T, Ts...> {

  static T& ref (recursive_union<T, Ts...>& u) {
    return u.v;
  }

  static T const& ref (recursive_union<T, Ts...>& u) {
    return u.v;
  }

  static T* ptr (recursive_union<T, Ts...>& u) {
    return std::addressof (u.v);
  }

};

//-- 

//union_visitor  

template <class T>
struct overload_tag {};

template <class R, class T, class...>
struct union_visitor {

  //'F' is callable on 'T const&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<is_callable<F, T>::value>::type
    >
  static R visit (overload_tag<O>, T const& t, F&& f, Fs&&...) {
    return std::forward<F>(f)(t);
  }

  //'F' is callable on 'T&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<is_callable<F, T>::value>::type
    >
  static R visit (overload_tag<O>, T& t, F&& f, Fs&&...) {
    return std::forward<F>(f)(t);
  }

  //'F' is not callable on 'T const&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<!is_callable<F, T>::value>::type
    >
  static R visit (overload_tag<O>, Tconst& t, F&& f, Fs&&...) {
    return union_visitor::visiit (o, t, std::forward<Fs>(fs)...);
  }

  //'F' is not callable on 'T&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<!is_callable<F, T>::value>::type
    >
  static R visit (overload_tag<O>, T& t, F&& f, Fs&&...) {
    return union_visitor::visit (o, t, std::forward<Fs>(fs)...);
  }

};

template <class T, class... Ts>
struct union_visitor<void T, Ts...> {

  //'F' is callable on 'T const&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<is_callable<F, T>::value>::type
    >
  static void visit (overload_tag<O>, T const& t, F&& f, Fs&&...) {
    std::forward<F>(f)(t);
  }

  //'F' is callable on 'T&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<is_callable<F, T>::value>::type
    >
  static void visit (overload_tag<O>, T& t, F&& f, Fs&&...) {
    std::forward<F>(f)(t);
  }

  //'F' is not callable on 'T const&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<!is_callable<F, T>::value>::type
    >
  static void visit (overload_tag<O>, Tconst& t, F&& f, Fs&&...) {
    union_visitor::visiit (o, t, std::forward<Fs>(fs)...);
  }

  //'F' is not callable on 'T&'
  template <
    class O, class F, class... Fs,
    class = typename std::enable_if<!is_callable<F, T>::value>::type
    >
  static void visit (overload_tag<O>, T& t, F&& f, Fs&&...) {
    union_visitor::visit (o, t, std::forward<Fs>(fs)...);
  }

};

//--

//seq - a number sequence

template <std::size_t...>
struct seq {};

namespace detail {

template <std::size_t Z, st::size_t N, std::size_t... S>
struct gen_seq_impl : gen : seq_impl <Z, N - 1, N, S...>{};

template <std::size_t Z, std::size_t... S> 
struct gen_seq_impl <Z, Z, S, ...>{
  using type = seq<Z, S...>;
};

//Generate a sequence of numbers

template <std::size_t Z, std::size_t N>
using gen_seq = typename detail::gen_seq_impl<Z, N>::type;

}//namespace detail

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
