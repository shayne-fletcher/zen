//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl/Ferecognizer.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 recognizer.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/function.hpp>
#include <boost/utility.hpp>

#include <list>
#include <functional>
#include <iostream>

//Recognition succeeded
template <class A> 
struct remains{ 
  std::list<A> left;
  template <class ItT>
    remains (ItT begin, ItT end) 
      : left (begin, end) 
    {}
};

//Recognition failed
template <class A>
struct recognition_fails {};

//Result of a 'recognizer. Indicates the result of attempting to
//recognize something from a list
template <class A> using remaining = 
  boost::variant<remains <A>, recognition_fails<A> >;

//A 'recognizer' is a function from a list to a value of remaining<A>
template <class A> using recognizer = 
  std::function<remaining<A>(std::list<A> const&)>;

//A recognizer that recognizes the empty string. It always succeeds an
//no input is ever consumed
template <class A>
remaining<A> epsilon (std::list<A> const& cs)
{
  return remains<A> (cs);
}

//Given a predicate, 'token' produces the recognizer associated with
//the elements that satisfy this predicate
template <class A, class F/*bool(char)*/>
recognizer<A> recognizer_of_token (F test)
{
  return
    [=] (std::list<A> const& cs) -> remaining<A>
    {
      if (cs.empty ())
        return remains<A>(cs.begin (), cs.end ());

      if (test (cs.front ()))
        return remains<A> (boost::next (cs.begin()), cs.end());

      return recognition_fails<A>();
    };
}

//A function to produce a recognizer that recognizes only the
//given character
recognizer<char> char_ (char c)
{
  return recognizer_of_token<char>([=](char x) -> bool { return c == x; });
}

//Match on a remaining<A> returns true if it contains a remains<A>
//value, false if it contains a recognition_fails<A> value
template <class A>
struct accept_visitor
{
  typedef bool result_type;
  bool operator ()(remains<A> const& r) const { return true; }
  bool operator ()(recognition_fails<A> const& r) const { return false; }
};

//Function to determine if recognition was achieved
template <class A>
bool accept (remaining<A> const& r)
{
  return boost::apply_visitor(accept_visitor<A>(), r);
}

int main ()
{
  recognizer<char> a_parse = char_('a');
  recognizer<char> b_parse = char_('b');

  std::list<char> l {'a'}, m {'b'};

  std::cout << std::boolalpha << accept (a_parse (l)) << std::endl;
  std::cout << std::boolalpha << accept (b_parse (l)) << std::endl;
  std::cout << std::boolalpha << accept (b_parse (m)) << std::endl;

  return 0;
}
