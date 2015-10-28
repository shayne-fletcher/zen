#if !defined(RECURISIVE_UNION_HEADER_3AF9CF8D_FC73_4F94_B4E3_ED88079ABC5D_H)
#  define RECURISIVE_UNION_HEADER_3AF9CF8D_FC73_4F94_B4E3_ED88079ABC5D_H

namespace foo {

template <class ...>
struct recursive_union {};

/*
  let rec drop l i =
    if i = 0 then l else
    match l with
     | [] -> []
     | (h :: tl) -> drop tl (i - 1)
*/

template <std::size_t I, class T, class... Ts>
struct union_indexer {

  static constexpr auto ref (recursive_union<T, Ts...>& u) 
    /*-> decltype (union_indexer<I - 1, Ts...>::ref (u.r))*/ {
    return union_indexer<I - 1, Ts...>::ref (u.r)
  }

  static constexpr auto ref (recursive_union<T, Ts...> const& u) 
    /*-> decltype (union_indexer<I - 1, Ts...>::ref (u.r))*/ {
    return union_indexer<I - 1, Ts...>::ref (u.r)
  }

  static constexpr auto ptr (recursive_union<T, Ts...> const& u) 
    /*-> decltype (union_indexer<I - 1, Ts...>::ptr (u.r))*/ {
    return union_indexer<I - 1, Ts...>::ptr (u.r)
  }

};

template <class T, class... Ts>
struct union_indexer<0, T, Ts...> {

  static constexpr T& ref (recursive_union<T, Ts...>& u) {
    return u.v;
  }

  static constexpr T const& ref (recursive_union<T, Ts...>& u) {
    return u.v;
  }

  static constexpr T* ptr (recursive_union<T, Ts...>& u) {
    return std::addressof (u.v);
  }

};

}//namespace foo

#endif //!defined(RECURISIVE_UNION_HEADER_3AF9CF8D_FC73_4F94_B4E3_ED88079ABC5D_H)
