//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl /Feruntime_length_encoding_compression_method.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 runtime_length_encoding_compression_method.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/range.hpp>
#include <boost/range/numeric.hpp>

#include <list>

//Represenation of the runtime encoding
template <class A> struct C { std::pair <int, A> item; };
template <class A> using datum = boost::variant <A, C<A>>;
template <class A> using encoding = std::list<datum<A>>;

//Procedural function object that modifies updates an encoding given a
//datum
template <class A>
struct update : boost::static_visitor<> {
  A c;
  encoding<A>& l;

  update (A c, encoding<A>& l) : c (c), l (l) 
  {}

  void operator ()(A e) const { 
    if (e == c) {
      l.back () = C<A>{ std::make_pair(2, c) }; 
      return;
    }
    l.push_back (c);
  }

  void operator ()(C<A> const& elem) const { 
    if (elem.item.second == c) {
      l.back () = C<A>{ std::make_pair (elem.item.first + 1, c) };
      return;
    }
    l.push_back (c);
  }
};

template <class R>
encoding<typename boost::range_value<R>::type> rle (R bytes) {
  typedef boost::range_value<R>::type A;

  auto f = [](encoding<A> acc, A b) -> encoding<A> {
    if (acc.size () == 0)
      acc.push_back (b);
    else   {
      boost::apply_visitor (update<A>(b, acc), acc.back ());
    }
    return acc;
  };

  return boost::accumulate (bytes, encoding<A> (), f);
}

#include <iostream>
#include <string>

int main () {
  std::string buf=
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    "c"
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
    "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    "z";
  std::list<char> data(buf.begin (), buf.end());
  encoding<char> compressed = rle (data);

  std::cout << sizeof (char) * (data.size ()) << std::endl;
  std::cout << sizeof (datum <char>) * (compressed.size ()) << std::endl;

  return 0;
}
