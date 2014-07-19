//"c:/users/fletch/desktop/setenv.cmd"
//cl /EHsc /Femerge.exe /I :/project/boost_1_55_0 merge.cpp 

#include <boost/next_prior.hpp>
#include <boost/range.hpp>

#include <cstdlib>

namespace algo
{
  template <class S>
  S take (std::size_t n, S src)
  {
    typedef boost::range_iterator<S>::type it_t;

    it_t curr = boost::begin (src), last = boost::end (src);

    if (n == 0 || boost::empty (src))
      return S (last, last);

    S r = take (n-1, S (boost::next (curr), last));

    return S (boost::begin (src), boost::end (r));
  }

}//namespace algo

#include <utility>
#include <algorithm>
#include <iterator>
#include <iostream>

int main ()
{
  int data[] = {1, 2, 3, 4};

  std::pair<int const*, int const*> r = 
    algo::take (2u, std::make_pair (data, data + 4));
  std::copy (boost::begin (r), boost::end (r), std::ostream_iterator<int>(std::cout, ", "));

  return 0;
}
