//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fel.exe /Zi /MDd /EHsc /I d:/boost_1_59_0 l2.cpp

#include <list>
#include <iterator>
#include <type_traits>
#include <algorithm>
#include <iostream>

/*
  The list monad.

  let unit : 'a -> 'a t = fun a -> [a]

  let rec ( * ) : 'a t -> ('a -> 'b t) -> 'b t =
    fun l -> fun k ->
      match l with | [] -> [] | (h :: tl) -> k h @ tl * k

*/

//The unit list containing 'a'

template <class A> 
std::list<A> unit (A const& a) { return std::list<A> (1u, a); }

//The list monad 'bind' operator

template <class A, class F>
typename std::result_of<F(A)>::type 
operator * (std::list<A> a, F k) {
  typedef typename std::result_of<F(A)>::type result_t;

  if (a.empty ())
    return result_t ();

  result_t res = k (a.front ());
  a.pop_front ();
  res.splice (res.end (), a * k);

  return res;
}

/*
    let join : 'a t t z = z * fun m -> m
*/

//'join' concatenates a list of lists

template <class A>
std::list <A> join (std::list<std::list<A>> const& z) {
  return z * [](auto m) { return m; };
}

/*
    let map : ('a -> b') -> 'a t -> 'b t =
      fun f -> fun m -> m * fun a -> unit (f a)
*/

//'map' is the equivalent of 'std::transform'

template <class A, class F>
std::list<A> map (F f, std::list<A> const& m) {
  return m * [=](auto a) { return unit (f (a)); };
}

namespace std {

  template <class A, class B>
  std::ostream& operator << (std::ostream& os, std::pair<A, B> const& p) {
    return os << "(" << p.first << ", " << p.second << ")";
  }

}//namespace std

int main () {

  std::list<int> tmp;

  //l = [1, 2, 3]
  std::list<int> l = {1, 2, 3};
  
  //m = [1, 4, 9]
  auto m = l * [](int x) { return unit (float (x * x)); };
  std::copy (m.begin (), m.end (), std::ostream_iterator<float>(std::cout, ","));
  std::cout << "\n";

  //n = l x m = [(1, 1), (1, 4), (1, 9), (2, 1), (2, 4), (2, 9), ...]
  auto n = l * ([&m](int x){ return m * ([=](float y){ return unit (std::make_pair (x, y)); });});
  std::copy (n.begin (), n.end (), std::ostream_iterator<std::pair<int, float>>(std::cout, ","));
  std::cout << "\n";

  //'join'
  std::list<std::list<int>> ll={{1, 2, 3}, {4, 5, 6}};
  tmp = join (ll);
  std::copy (tmp.begin (), tmp.end (), std::ostream_iterator<int>(std::cout, ","));
  std::cout << "\n";

  //'map'
  tmp = map ([](auto x) { return x*x; }, l);
  std::copy (tmp.begin (), tmp.end (), std::ostream_iterator<int>(std::cout, ","));
  std::cout << "\n";

  return 0;
}
