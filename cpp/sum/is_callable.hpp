#if !defined (IS_CALLABLE_677D7BAE_8A3C_41A6_ADA5_3ED43F6B1268_H)
#  define IS_CALLABLE_677D7BAE_8A3C_41A6_ADA5_3ED43F6B1268_H

#  include <type_traits>

namespace foo {

  struct no {};

  template<typename F, typename...Args>
  struct is_callable {

    template<class G, class...Qs>
    static auto check(G g, Qs...qs) -> decltype(g(qs...));

    static no check(...);

    static constexpr bool value = 
     !std::is_same<
        no
      , decltype(check (std::declval<F>(), std::declval<Args>()...))>::value;

  	//using type =
  	//	decltype(check(std::declval<F>(), std::declval<Args>()...));
  };

}//namespace foo

#endif //!defined (IS_CALLABLE_677D7BAE_8A3C_41A6_ADA5_3ED43F6B1268_H)
