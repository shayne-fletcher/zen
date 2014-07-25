//"c:/program files (x86)/Microsoft Visual Studio 10.0/vc/vcvarsall.bat" x64
//cl /Fecartesian.exe /I d:/boost_1_55_0 cartesian.cpp

#include <boost/range.hpp>

#include <numeric>
#include <ostream>

template <class R1, class R2, class ItT>  
ItT prod (R1 A, R2 B, ItT dst) {

  typedef ItT iterator;
  typedef boost::range_value<R1>::type alpha;
  typedef boost::range_value<R2>::type  beta;

  return std::accumulate (boost::begin (A), boost::end (A), dst, 
    [=] (iterator acc, alpha const& a) { 
      return std::accumulate (boost::begin (B), boost::end (B), acc, 
        [=] (ItT acc, beta const& x) { return *acc++ = std::make_pair (a, x); });
     });
}

// --

//Test : A = {a, b} and B = {c, d}

namespace std
{
  template <class A, class B>
  std::ostream& operator << (std::ostream& os, std::pair<A, B> const& p)
  {
    os << '(' << p.first << ", " << p.second << ')';

    return os;
  }
}

int main ()
{
  using boost::make_iterator_range;

  char const A[] = {'a', 'b'}, B[] = {'c', 'd'};
  std::ostream_iterator<std::pair<char, char> > out(std::cout, ", ");

  prod (make_iterator_range (A, A + 2), make_iterator_range (B, B + 2), out);

  return 0;
}

