//"c:/users/fletch/desktop/setenv.cmd"
//cl /EHsc /Femerge.exe /I :/project/boost_1_55_0 merge.cpp 

#include <boost/next_prior.hpp>
#include <boost/range.hpp>
#include <boost/range/algorithm/copy.hpp>

#include <vector>
#include <cstdlib>

namespace algo
{
  //take

  template <class S, class D>
  D take (std::size_t n, S src, D dst)
  {
    typedef boost::range_iterator<S>::type it_t;
    it_t curr = boost::begin (src), last = boost::end (src);
    if (n <= 0) return dst;
    if (boost::empty (src))  return dst;
    take (n-1, S (boost::next (curr), last), *dst++ = *curr);
    return dst;
  }

  //drop

  template <class S, class D>
  D drop (std::size_t n, S src, D dst)
  {
    typedef boost::range_iterator<S>::type it_t;
    it_t curr = boost::begin (src), last = boost::end (src);
    if (n <= 0) return boost::range::copy (src, dst);
    if (boost::empty (src))return dst;
    drop (n-1, S (boost::next (curr), last), dst);
    return dst;
  }

  //split

  template <class S, class D1, class D2>
  void split (int n, S src, D1 dst1, D2 dst2)
  { take (n, src, dst1); drop (n, src, dst2); }

  //merge

  template <class S1, class S2, class D>
  D merge (S1 src1, S2 src2, D dst)
  {
    typedef boost::range_iterator<S1>::type it1_t;
    typedef boost::range_iterator<S2>::type it2_t;
    typedef std::iterator_traits<it1_t>::value_type value1_t;
    typedef std::iterator_traits<it2_t>::value_type value2_t;
    if (boost::empty (src1)) return boost::copy (src2, dst);
    if (boost::empty (src2)) return boost::copy (src1, dst);
    it1_t curr1 = boost::begin (src1), last1 = boost::end (src1);
    it2_t curr2 = boost::begin (src2), last2 = boost::end (src2);
    value1_t x = *curr1;
    value2_t y = *curr2;
    if (x <= y)
      merge (S1 (boost::next (curr1), last1), src2, *dst++ = x);
    else
      merge (src1, S2 (boost::next (curr2), last2), *dst++ = y);
    return dst;
  }

  //merge_sort

  template <class S, class D>
  D merge_sort (S src, D dst)
  {
    typedef boost::range_iterator<S>::type it_t;
    typedef std::iterator_traits<it_t>::value_type value_t;
    std::size_t i = boost::size (src)/2;
    if (i == 0) return boost::range::copy (src, dst);
    std::vector<value_t> u, v;
    split (i, src, std::back_inserter (u), std::back_inserter (v));
    std::vector<value_t> xs, ys;
    merge_sort (u, std::back_inserter (xs));
    merge_sort (v, std::back_inserter (ys));
    merge (xs, ys, dst);
    return dst;
  }
  
}//namespace algo

#include <boost/assign/list_of.hpp>

#include <utility>
#include <algorithm>
#include <iterator>
#include <iostream>

int main ()
{
  using std::pair;
  using std::make_pair;
  using std::ostream_iterator;
  using boost::assign::list_of;

  int ord[] = {1, 2, 3, 4};

  auto src=make_pair(ord, ord + 4);
  auto dst=ostream_iterator<int>(std::cout, ", ");

  std::cout << "\ntake ():\n";

  algo::take (0u, src, dst); std::cout << std::endl;
  algo::take (1u, src, dst); std::cout << std::endl;
  algo::take (2u, src, dst); std::cout << std::endl;
  algo::take (3u, src, dst); std::cout << std::endl;
  algo::take (4u, src, dst); std::cout << std::endl;
  algo::take (5u, src, dst); std::cout << std::endl;

  std::cout << "\ndrop ():\n";

  algo::drop (5u, src, dst); std::cout << std::endl;
  algo::drop (4u, src, dst); std::cout << std::endl;
  algo::drop (3u, src, dst); std::cout << std::endl;
  algo::drop (2u, src, dst); std::cout << std::endl;
  algo::drop (1u, src, dst); std::cout << std::endl;
  algo::drop (0u, src, dst); std::cout << std::endl;

  std::cout << "\nsplit ():\n\n";

  algo::split (0u, src, dst, dst); std::cout << std::endl;
  algo::split (1u, src, dst, dst); std::cout << std::endl;
  algo::split (2u, src, dst, dst); std::cout << std::endl;
  algo::split (3u, src, dst, dst); std::cout << std::endl;
  algo::split (4u, src, dst, dst); std::cout << std::endl;
  algo::split (5u, src, dst, dst); std::cout << std::endl;

  std::cout << "\nmerge ():\n\n";

  std::vector <int> l=list_of(-1)(2), r=list_of(0)(1);
  algo::merge (l, r, dst); std::cout << std::endl;

  std::cout << "\nmerge_sort ():\n\n";

  int unord[] = {-1, 2, 0, 4, 3, 5};
  algo::merge_sort (make_pair (unord, unord + 6), dst);

  return 0;
}
