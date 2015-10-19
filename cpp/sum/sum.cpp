//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Feu.exe /Zi /MDd /EHsc /I d:/boost_1_59_0 u.cpp

#include <boost/core/enable_if.hpp>
#include <boost/mpl/and.hpp>
#include <boost/type_traits/is_same.hpp>

#include <cstddef>
#include <string>
#include <iostream>

template <class T>
class recursive_wrapper;

template <class T>
struct is_recursive_wrapper : boost::mpl::false_ {};

template <class T>
struct is_recursive_wrapper<recursive_wrapper<T>> : boost::mpl::true_ {};

template <typename T>
struct unwrap_recursive {
    typedef T type;
};

template <typename T>
struct unwrap_recursive< recursive_wrapper<T> > {
  typedef T type;
};

template <class T>
class recursive_wrapper {
private:
  T* p_;
  recursive_wrapper& assign (T const& rhs) { this->get() = rhs; return *this; }

public:
  typedef T type;

  template <class... Args> recursive_wrapper (Args&&... args) : p_(new T (std::forward<Args>(args)...)) {}
  recursive_wrapper (recursive_wrapper const& rhs) : p_ (new T (rhs.get ())) {}
  recursive_wrapper (T const& rhs) : p_ (new T (rhs)) {}
  recursive_wrapper(recursive_wrapper&& rhs) : p_ (new T (std::move (rhs.get ()))) {}
  recursive_wrapper(T&& rhs) : p_ (new T (std::move (rhs))) {}
  ~recursive_wrapper() {  delete p_; }

  recursive_wrapper& operator=(recursive_wrapper const& rhs){ return assign (rhs.get()); }
  recursive_wrapper& operator=(T const& rhs) { return assign (rhs); }
  void swap(recursive_wrapper& rhs) noexcept { std::swap (p_, rhs.p_); }
  recursive_wrapper& operator=(recursive_wrapper&& rhs) noexcept { swap (rhs); return *this; }
  recursive_wrapper& operator=(T&& rhs) { get() = std::move (rhs); return *this; }

  T& get() { return *get_pointer(); }
  T const& get() const { return *get_pointer(); }
  T* get_pointer() { return p_; }
  T const* get_pointer() const { return p_; }
};

template <typename T>
inline void swap(recursive_wrapper<T>& lhs, recursive_wrapper<T>& rhs) noexcept {
  lhs.swap(rhs);
}

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

  template <class... Args>
  explicit recursive_union (constructor<T>, Args&&... args) 
    : v (std::forward<Args>(args)...)
  {}

  template <class U, class... Args,
    typename boost::enable_if<
      boost::mpl::and_<
        is_recursive_wrapper<T>
      , boost::is_same<U, typename unwrap_recursive<T>::type>>, int>::type = 0
  >
  explicit recursive_union (constructor<U>, Args&&... args)
  : v (std::forward<Args>(args)...)
  {}

  template <class U, class... Args,
    typename boost::disable_if<
      boost::mpl::and_<
        is_recursive_wrapper<T>
      , boost::is_same<U, typename unwrap_recursive<T>::type>>, int>::type = 0
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

struct E_const;
struct E_add;

using expression = sum_type<E_const, recursive_wrapper<E_add>>;

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
     constructor<E_add>()
   , expression (constructor<E_const>(), 2)
   , expression (constructor<E_const>(), 3));

  return 0;
}
