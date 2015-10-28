#if !defined (RANGE_B50055B9_6E77_4213_B3CE_31BD8A147BE1_H)
#  define RANGE_B50055B9_6E77_4213_B3CE_31BD8A147BE1_H

# include <cstddef>

namespace foo {

template <std::size_t...>
struct range {};

/*
  Similar to,

  let range z e =
    let rec aux acc k = 
      if k < z then acc
      else aux (k :: acc) (k - 1) in 
     aux [] (e - 1)
*/

namespace detail {

template <std::size_t Z, std::size_t N, std::size_t... Ns>
struct mk_range  : mk_range <Z, N - 1, N, Ns...>
{};

template <std::size_t Z, std::size_t... Ns>
struct mk_range<Z, Z, Ns...> {
  using type = range<Z, Ns...>;
};

}//namespace detail

template <std::size_t Z, std::size_t N>
using mk_range = typename detail::mk_range<Z, N>::type;

}//namespace foo

#endif //!defined (RANGE_B50055B9_6E77_4213_B3CE_31BD8A147BE1_H)
