#if !defined (SUM_81D195F4_819A_4E24_A6BF_E9630B3495E8_H)
#  define SUM_81D195F4_819A_4E24_A6BF_E9630B3495E8_H

#  include "recursive_union.hpp"

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

  template <class R, class... Fs>
  R match(Fs&&... fs) const {
    using indicies = mk_range<0, sizeof... (Ts) - 1>;

    return union_visitor<R, indicies, Ts...>::visit (
                  data, cons, std::forward<Fs>(fs)...);
  }

  template <class R, class... Fs>
  R match(Fs&&... fs) {
    using indicies = mk_range<0, sizeof... (Ts) - 1>;

    return union_visitor<R, indicies, Ts...>::visit (
                  data, cons, std::forward<Fs>(fs)...);
  }

  template <class... Fs>
  void match(Fs&&... fs) const {
    using indicies = mk_range<0, sizeof... (Ts) - 1>;

    union_visitor<void, indicies, Ts...>::visit (
                  data, cons, std::forward<Fs>(fs)...);
  }

  template <class... Fs>
  void match(Fs&&... fs) {
    using indicies = mk_range<0, sizeof... (Ts) - 1>;
   
    union_visitor<void, indicies, Ts...>::visit (
                  data, cons, std::forward<Fs>(fs)...);
  }

};

}//namespace foo

#endif //!defined (SUM_81D195F4_819A_4E24_A6BF_E9630B3495E8_H)
