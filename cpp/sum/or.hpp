#if !defined (OR_AEFEBE20_B03C_4C0F_BC77_187BCB64C385_H)
#  define OR_AEFEBE20_B03C_4C0F_BC77_187BCB64C385_H

/*

let or_ l =
  let rec loop f acc l =
     match l with
     | [] -> acc
     | (h :: tl) -> loop f (f acc h) tl in
  loop (fun acc i -> acc || i) false l

*/

namespace foo {

namespace detail {

template <bool acc, class... Ts>
struct or_aux {
  static constexpr bool const value = acc;
};

template <bool acc, class T, class... Ts>
struct or_aux<acc, T, Ts...> : or_aux <acc || T::value, Ts...> {
};

}//namespace detail

template <class... Ts>
struct or_ : detail::or_aux <false, Ts...> {};

}//namespace foo

#endif //!defined (OR_AEFEBE20_B03C_4C0F_BC77_187BCB64C385_H)
