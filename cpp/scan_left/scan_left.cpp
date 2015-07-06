//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fescan_left.exe /Zi /MDd /EHsc /I d:/boost_1_56_0 scan_left.cpp

#include <boost/range.hpp>

#include <iostream>
#include <iterator>
#include <vector>

template <
    class F
  , class AccT
  , class RngT
  , class OutT 
> 
OutT scan_left (F f, AccT z, RngT xs, OutT out) {
  *out++ = z;
  if (boost::empty (xs)){
    return out;
  }

  auto first = boost::begin (xs);
  auto x = *first;
  auto begin = boost::next (first);
  auto const end_ = boost::end (xs);
  
  return scan_left (
     f
   , f (z, x)
   , std::make_pair (begin, end_)
   , out
   );
}

int main () {

  auto _ = scan_left (
       [] (int z, int x) { return z + x; }
     , 0
     , std::vector<int>({1, 2, 3})
     , std::ostream_iterator<int>(std::cout, "\n"));

  return 0;
}
