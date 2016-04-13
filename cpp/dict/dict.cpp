#include <pgs/pgs.hpp>

#include <functional>
#include <iostream>
#include <cstdint>

using namespace BloombergLP;
using namespace pgs;

// -- A rough and ready `'a option` (given the absence of
// `std::experimental::optional`

struct None {};

template <class A>
struct Some { 
  A val;
  template <class Arg>
  explicit Some (Arg&& s) : val { std::forward<Arg> (s) }
  {}
};

template <class B>
using option = sum_type<None, Some<B>>;

template <class B>
std::ostream& operator << (std::ostream& os, option<B> const& o) {
  return o.match<std::ostream&>(
    [&](Some<B> const& a) -> std::ostream& { return os << a.val; },
    [&](None) -> std::ostream& { return os << "<empty>"; }
  );
}

//-- Encoding of dictionaries as functions

template <class K, class V>
using dict_type = std::function<option<V>(K)>;

//`empty` is a dictionary constant (a function that maps any key to
//`None`)
template <class A, class B>
dict_type<A, B> empty = 
  [](A const&) { 
    return option<B>{ constructor<None>{} }; 
};

//`add (d, k, v)` extends `d` with a binding of `k` to `v`
template <class A, class B>
dict_type<A, B> add (dict_type<A, B> const& d, A const& k, B const& v) {
  return [=](A const& l) {
    return (k == l) ? option<B>{ constructor<Some<B>>{}, v} : d (l);
  };
}

//`find (d, k)` searches for a binding in `d` for `k`
template <class A, class B>
option<B> find (dict_type<A, B> const& d, A const& k) {
  return d (k);
}

//-- Test driver

int main () {

  using dict_t = dict_type<std::string, std::int64_t>;

  auto nil = empty<std::string, std::int64_t>;
  auto insert  =
    static_cast<
      dict_t(*)(dict_t const&, std::string const&, std::int64_t const&)
    >(add)
    ;

  dict_t despicable = 
    insert (
      insert (
        insert (nil
           , std::string {"Felonius Gru"}, std::int64_t{53})
           , std::string {"Dave the Minion"}, std::int64_t{4530000000})
          , std::string {"Dr. Joseph Albert Nefario"}, std::int64_t{80})
     ;

  std::cout << 
    find (despicable, std::string {"Dave the Minion"}) << std::endl;

  return 0;
}
