//"c:/users/fletch/desktop/setenv.cmd"
//cl /EHsc /Femerge.exe /I :/project/boost_1_55_0 merge.cpp 

#include <boost/next_prior.hpp>
#include <boost/range.hpp>

#include <algorithm>
#include <cstdlib>

namespace algo
{
  template <class S, class D>
  D take (std::size_t n, S src, D dst)
  {
    typedef boost::range_iterator<S>::type it_t;

    it_t curr = boost::begin (src), last = boost::end (src);

    if (n <= 0)
      return dst;

    if (boost::empty (src))
      return dst;

    take (n-1, S (boost::next (curr), last), *dst++ = *curr);

    return dst;
  }

  template <class S, class D>
  D drop (std::size_t n, S src, D dst)
  {
    typedef boost::range_iterator<S>::type it_t;

    it_t curr = boost::begin (src), last = boost::end (src);

    if (n <= 0)
      {
        std::copy (boost::begin (src), boost::end (src), dst);

        return dst;
      }

    if (boost::empty (src))
      return dst;

    drop (n-1, S (boost::next (curr), last), dst);
 
    return dst;
  }

}//namespace algo

#include <utility>
#include <algorithm>
#include <iterator>
#include <iostream>

int main ()
{
  int data[] = {1, 2, 3, 4};

  algo::take (2u, std::make_pair (data, data + 4), std::ostream_iterator<int>(std::cout, ", "));
  std::cout << std::endl;
  algo::drop (2u, std::make_pair (data, data + 4), std::ostream_iterator<int>(std::cout, ", "));

  return 0;
}
