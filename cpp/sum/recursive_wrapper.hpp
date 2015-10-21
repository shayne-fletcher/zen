#if !defined (RECURSIVE_WRAPPER_B2AE13A9_EF98_4630_B5C5_54983411750C_H)
#  define RECURSIVE_WRAPPER_B2AE13A9_EF98_4630_B5C5_54983411750C_H

// Copyright Eric Friedman, Itay Maman 2002-2003

#include <type_traits>

namespace foo {

template <class T>
class recursive_wrapper;

template <class T>
struct is_recursive_wrapper : std::false_type {};

template <class T>
struct is_recursive_wrapper<recursive_wrapper<T>> : std::true_type {};

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

}//namespace foo
#endif //!defined (RECURSIVE_WRAPPER_B2AE13A9_EF98_4630_B5C5_54983411750C_H)
