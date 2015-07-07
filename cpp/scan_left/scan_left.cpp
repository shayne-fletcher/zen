//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fescan_left.exe /Zi /MDd /EHsc /I d:/boost_1_56_0 scan_left.cpp

#include <boost/range.hpp>

#include <iostream>
#include <iterator>
#include <vector>
#include <numeric>
#include <algorithm>
#include <string>
#include <list>

template <
  class F, class AccT, class RngT, class OutT>
OutT scan_left (F f, AccT z, RngT xs, OutT out) {
  *out++ = z;
  if (boost::empty (xs)) return out;

  auto first = boost::begin (xs);
  auto const& x = *first;
  auto begin = boost::next (first);
  auto const end_ = boost::end (xs);

  return scan_left (f, f (z, x), std::make_pair (begin, end_), out);
}

template <class Rng1T, class Rng2T>
bool prefix (Rng1T ws, Rng2T s) {
  if (boost::empty (ws)) return true;
  if (boost::empty (s)) return false;
  return *boost::begin (ws) == *boost::begin (s) && 
     prefix (std::make_pair(
               boost::next(boost::begin(ws)), boost::end(ws))
           , std::make_pair (
               boost::next(boost::begin (s)), boost::end (s)));
}

template <class PredT, class RngT, class OutT>
OutT filter (PredT p, RngT xs, OutT out) {
  auto f = [&p](auto dst, auto const& x) { 
    if (p (x))
      *dst++ = x;

    return dst;
  };

  return std::accumulate (boost::begin (xs), boost::end (xs), out, f);
}

template <class OutT>
OutT matches (std::string const& ws, std::string const& s, OutT dst) {
  typedef std::pair<int, std::list<char>> acc_t;
  auto step = [](acc_t p, char x) -> acc_t {
    p.first += 1;
    p.second.push_front (x);

    return p;
  };

  std::string sw(ws.rbegin (), ws.rend ());

  std::list<acc_t> buf;
  scan_left (
      step
    , std::make_pair(0, std::list<char>())
    , s
    , std::back_inserter(buf));

  std::list<acc_t> temp;
  auto pred = [&sw] (auto p) -> bool { return prefix (sw, p.second); };
  filter (pred, buf, std::back_inserter (temp));

  return std::transform (temp.begin (), temp.end (), dst, 
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
