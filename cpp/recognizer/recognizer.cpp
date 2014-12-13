//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl /Ferecognizer.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 recognizer.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/utility.hpp>

#include <list>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>

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

//Result of a recognizer. Indicates the result of attempting to
//recognize something from a list
template <class A> using remaining = 
  boost::variant<remains <A>, recognition_fails<A> >;

//A 'recognizer' is a function from a list to a value of remaining<A>
template <class A> using recognizer = 
  std::function<remaining<A>(std::list<A> const&)>;

//A recognizer that recognizes the empty string. It always succeeds and
//no input is ever consumed
template <class A>
recognizer<A> epsilon ()
{
  return [](std::list<A> const& cs) -> remaining<A>
    {
      return remains<A> (cs.begin (), cs.end ());
    };
}

//Given a predicate, 'recognizer_of_token' produces the recognizer
//associated with the elements that satisfy this predicate
template <class A, class F/*bool(char)*/>
recognizer<A> recognizer_of_token (F test)
{
  return
    [=] (std::list<A> const& cs) -> remaining<A>
    {
      if (cs.empty ())
        return recognition_fails<A> ();

      if (test (cs.front ()))
        return remains<A> (boost::next (cs.begin()), cs.end());

      return recognition_fails<A>();
    };
}

//Recognizer disjunction
template <class A>
recognizer<A> compose_or (recognizer<A> p, recognizer<A> q)
{
  return [=](std::list<A> const& toks) -> remaining<A>
    {
      remaining <A> res = p (toks);
      if (remains<A>* rem = boost::get<remains<A> >(&res)) return *rem;

      return q (toks);
    };
}

//Recognizer conjunction
template <class A>
recognizer<A> compose_and (recognizer<A> p, recognizer<A> q)
{
  return [=](std::list<A> const& toks) -> remaining<A>
    {
      remaining <A> res = p (toks);
      if (remains<A>* rem = boost::get<remains<A> >(&res)) return q (rem->left);

      return recognition_fails<A> ();
    };
}

//The '*' iterator (Kleen star)
template <class A>
recognizer<A> zero_or_more (recognizer<A> p)
{
  return [=] (std::list<A> const& toks) -> remaining <A>
    {
      return compose_or (compose_and (p, zero_or_more<A> (p)) , epsilon<A> ()) (toks);
    };
}

//A function to produce a recognizer that recognizes only the
//given character
recognizer<char> char_ (char c)
{
  return recognizer_of_token<char>([=](char x) -> bool { return c == x; });
}

//A function to produce a recognizer of a specific string
recognizer<char> string_ (std::string const& s)
{
  std::list <recognizer <char> > rs;

  typedef std::back_insert_iterator<std::list<recognizer<char> > > it_t;

  std::accumulate (
      s.begin ()
    , s.end ()
    , std::back_inserter (rs)
    , [](it_t dst, char c) -> it_t 
      { *dst++ = char_ (c); return dst; });

  return std::accumulate (
      boost::next (rs.begin ())
    , rs.end ()
    , rs.front ()
    , [](recognizer<char> acc, recognizer<char> r) -> recognizer<char>
      { return compose_and (acc, r); });
}

//Match on a remaining<A> returns 'true' if it contains a 'remains<A>'
//value, 'false' if it contains a 'recognition_fails<A>' value
template <class A>
struct accept_visitor
{
  typedef bool result_type;
  bool operator () (remains<A> const& r) const { return r.left.empty (); }
  bool operator () (recognition_fails<A> const& r) const { return false; }
};

//Function to determine if recognition was achieved
template <class A>
bool accept (remaining<A> const& r)
{
  return boost::apply_visitor( accept_visitor<A> (), r);
}

//Test if the provided recognizer can recognize the given string
bool parse (recognizer<char> parser, std::string const& s)
{
  return accept (parser (std::list<char>(s.begin (), s.end() ));
}

int main ()
{
  std::cout.setf(std::ios_base::boolalpha);

  std::cout << parse (char_ ('a'), "a") << std::endl; //true
  std::cout << parse (char_ ('b'), "a") << std::endl; //false
  std::cout << parse (char_ ('b'), "b") << std::endl; //true

  //a*
  std::cout << parse (zero_or_more (char_ ('a')), "aaa") << std::endl; //true
  std::cout << parse (zero_or_more (char_ ('a')), "") << std::endl; //true
  std::cout << parse (zero_or_more (char_ ('a')), "ab") << std::endl; //false

  std::cout << parse (string_ ("foo"), "foo") << std::endl; //true
  std::cout << parse (string_ ("foo"), "bar") << std::endl; //false
  
  return 0;
}
