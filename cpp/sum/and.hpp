#if !defined(AND_D196A358_E369_4AAC_8AFA_F6B93CAB256C_H)
#  define AND_D196A358_E369_4AAC_8AFA_F6B93CAB256C_H

#  include <type_traits>

namespace foo {

//Credit : Jonathan Wakely

template <class... Ts>
struct and_ : std::true_type {
};

template <class T, class... Ts>
struct and_<T, Ts...> 
  : std::conditional<T::value, and_<Ts...>, std::false_type>::type {
};

}//namespace foo

#endif //!defined(BOOL_D196A358_E369_4AAC_8AFA_F6B93CAB256C_H)
