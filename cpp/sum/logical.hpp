#if !defined(LOGICAL_H)
#  define LOGICAL_H

#  include <type_traits>

namespace pgs {

template<class F, class Acc, class... Ts>
struct fold_left : Acc {
};

template <class F, class Acc, class T, class... Ts>
struct fold_left<F, Acc, T, Ts...> : 
    fold_left <F, typename F::template apply<Acc, T>::type, Ts...> {
};

//or

struct or_helper {
  template <class Acc, class T>
  struct apply : std::integral_constant<bool, Acc::value || T::value> {
  };
};

template <class... Ts>
struct or_ : fold_left <or_helper, std::false_type, Ts...> {
};

//and

struct and_helper {
  template <class Acc, class T>
  struct apply : std::integral_constant<bool, Acc::value && T::value> {
  };
};

template <class... Ts>
struct and_ : fold_left <and_helper, std::true_type, Ts...> {
};

}//namespace pgs

#endif//!defined(LOGICAL_H)
