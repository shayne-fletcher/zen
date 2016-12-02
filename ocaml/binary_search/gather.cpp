//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fegather.exe /Zi /MDd /EHsc gather.cpp

#include <iterator>
#include <tuple>
#include <iostream>
#include <vector>

//`partition (p, begin, end, xs, ys)` computes a partitioning of the
//sequence `[begin, end)` using the predicate `p`. Those elements that
//satisfy `P` are written to `xs`, those that don't, `ys`
template <class P, class S, class D>
auto partition (P p, S begin, S end, D xs, D ys) ->
  decltype (std::make_tuple (std::declval<D> (), std::declval<D>()))
{
  if (begin == end)
    return std::make_tuple (xs, ys);
  auto const& h = *begin++;
  *(p (h) ? xs : ys)++ = h;
  return partition (p, begin, end, xs, ys);
}

int main () {

  char const cs[] = {'a', 'a', 'b', 'c', 'c', 'b', 'a'};

  std::vector<char> xs, ys;
  partition (
    [](char c){ return c == 'a'; }
  , cs, cs + 7
  , std::back_inserter (xs)
  , std::back_inserter (ys));

  std::cout << "xs : ";
  std::copy (xs.begin (), xs.end ()
             , std::ostream_iterator<char>(std::cout, " "));
  std::cout << std::endl;

  std::cout << "ys : ";
  std::copy (ys.begin (), ys.end ()
             , std::ostream_iterator<char>(std::cout, " "));
  std::cout << std::endl;

  return 0;
}
