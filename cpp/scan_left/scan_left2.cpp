//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /F16777216 /Fescan_left.exe /F16777216 /Zi /MDd /EHsc /I d:/boost_1_56_0 scan_left.cpp

#include <boost/timer.hpp>

#include <iostream>
#include <iterator>
#include <vector>
#include <numeric>
#include <algorithm>
#include <string>
#include <deque>
#include <list>
#include <fstream>
#include <sstream>

/*
template <
  class F, class AccT, class InT, class OutT>
OutT scan_left (F f, AccT z, InT begin, InT end, OutT out) {
  *out++ = z;
  if (begin == end) return out;
  auto const& x = *begin;

  return scan_left (f, f (z, x), std::next (begin), end, out);
}
*/

template <
  class F, class AccT, class InT, class OutT>
OutT scan_left (F f, AccT z, InT begin, InT end, OutT out) {
loop:
  *out++ = z;
  if (begin == end) return out;
  auto const& x = *begin;
  z = f (z, x);
  ++begin;
  goto loop;
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
  typedef std::string::const_reverse_iterator it;
  typedef std::pair<it, it> iterator_range;
  typedef std::pair<int, iterator_range> acc_t;

  std::size_t num_chars=s.size();

  auto step = [num_chars,&s](acc_t p, char x) -> acc_t {
    ++p.first;
    std::string::const_reverse_iterator rbegin = s.rbegin ();
    std::advance (rbegin, num_chars - p.first);
    p.second = std::make_pair (rbegin, s.rend ());

    return p;
  };

  std::deque<acc_t> buf1;
  scan_left (
      step
    , std::make_pair (0, std::make_pair (s.rend (), s.rend()))
    , s.begin ()
    , s.end ()
    , std::back_inserter (buf1));

  std::string sw(ws.rbegin (), ws.rend ());
  auto pred = [num_chars, &sw, &s] (auto p) -> bool { 
    return prefix (sw.begin (), sw.end (), p.second.first, p.second.second); 
  };

  std::deque<acc_t> buf2;
  filter (pred, buf1, std::back_inserter (buf2));
  buf2.swap (buf1);

  return std::transform (buf1.begin (), buf1.end (), 
           dst, [](acc_t const& p) -> int {  return p.first; });
}

template <class OutT>
OutT matches2 (std::string const& ws, std::string const& s, OutT dst) {
  std::string sw (ws.rbegin (), ws.rend ());
  std::size_t num_chars=s.size ();
  for (std::size_t i = 0; i < num_chars; ++i) {
    std::string::const_reverse_iterator rbegin = s.rbegin ();
    std::advance (rbegin, num_chars - i);
    if (prefix (sw.begin (), sw.end (), rbegin, s.rend ()))  {
      *dst++ = i;
    }
  }

  return dst;
}

int test_basic () {
  std::list<int> where;
  matches ("abcab", "ababcabcab", std::back_inserter (where));

  std::for_each (where.begin (), where.end ()
   , [](int i) -> void { std::cout << i << ", "; }
   );  

  std::cout << std::endl;
}

int test_war_and_peace_matches () {

  std::ifstream ifs("war_and_peace");
  std::string text;
  ifs.seekg (0, std::ios::end);
  text.reserve (ifs.tellg());
  ifs.seekg (0, std::ios::beg);
  text.assign(
   (std::istreambuf_iterator<char>(ifs))
   , std::istreambuf_iterator<char>());

  std::cout << "File read. Searching..." << std::endl;

  std::list<int> where;
  matches ("people", text, std::back_inserter (where));
  
  std::cout << "There are " << where.size () << " occurences : ";
  
  std::for_each (where.begin (), where.end ()
   , [](int i) -> void { std::cout << i << ", "; }
  );  

  std::cout << std::endl;

  return 0;
}

int test_war_and_peace_matches2 () {

  std::ifstream ifs("war_and_peace");
  std::string text;
  ifs.seekg (0, std::ios::end);
  text.reserve (ifs.tellg());
  ifs.seekg (0, std::ios::beg);
  text.assign(
   (std::istreambuf_iterator<char>(ifs))
   , std::istreambuf_iterator<char>());

  std::cout << "File read. Searching..." << std::endl;

  std::list<int> where;
  matches2 ("people", text, std::back_inserter (where));
  
  // std::cout << "There are " << where.size () << " occurences : ";
  
  // std::for_each (where.begin (), where.end ()
  //  , [](int i) -> void { std::cout << i << ", "; }
  // );  

  // std::cout << std::endl;

  return 0;
}

int main () {

  //test_basic ();
  //test_war_and_peace_matches ();

  boost::timer t;
  for (std::size_t i = 0; i < 10; ++i)
    test_war_and_peace_matches2 ();
  double elapsed = t.elapsed ();
  std::cout << "time taken : " << elapsed/10.0 << " second\n";

  return 0;
}
