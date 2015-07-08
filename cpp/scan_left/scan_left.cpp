//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fescan_left.exe /Zi /MDd /EHsc /I d:/boost_1_56_0 scan_left.cpp

#include <iostream>
#include <iterator>
#include <vector>
#include <numeric>
#include <algorithm>
#include <string>
#include <deque>
#include <list>

template <
  class F, class AccT, class InT, class OutT>
OutT scan_left (F f, AccT z, InT begin, InT end, OutT out) {
  *out++ = z;
  if (begin == end) return out;
  auto const& x = *begin;

  return scan_left (f, f (z, x), std::next (begin), end, out);
}

template <class InT1, class InT2>
bool prefix (InT1 lb, InT1 le, InT2 rb, InT2 re) {
  if (lb == le) return true;
  if (rb == re) return false;

  return *lb == *rb && prefix (std::next (lb), le, std::next (rb), re);
}

template <class PredT, class RngT, class OutT>
OutT filter (PredT p, RngT xs, OutT out) {
  return std::accumulate (
      std::begin (xs), std::end (xs), out, 
      [&p](auto dst, auto const& x) { return p (x) ? *dst++ = x : dst ;});
}

template <class OutT>
OutT matches (std::string const& ws, std::string const& s, OutT dst) {
  typedef std::pair<int, std::deque<char>> acc_t;

  auto step = [](acc_t p, char x) -> acc_t {
    ++p.first;
    p.second.push_front (x);

    return p;
  };

  std::deque<acc_t> buf;
  scan_left (
      step
    , std::make_pair(0, std::deque<char>())
    , s.begin ()
    , s.end ()
    , std::back_inserter(buf));

  std::string sw(ws.rbegin (), ws.rend ());
  auto pred = [&sw] (auto p) -> bool { 
    return prefix (
      sw.begin (), sw.end ()
    , p.second.begin (), p.second.end ()); 
  };
  std::deque<acc_t> temp;
  filter (pred, buf, std::back_inserter (temp));

  return std::transform (
     temp.begin (), temp.end (), dst, 
     [](acc_t const& p) -> int {  return p.first; });
}

int main () {

  std::list<int> where;
  matches ("abcab", "ababcabcab", std::back_inserter (where));

  std::for_each (where.begin (), where.end ()
   , [](int i) -> void { std::cout << i << ", "; }
   );  

  return 0;
}
