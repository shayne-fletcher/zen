//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Ferecognizer.exe /Zi /MDd /EHsc /I d:/boost_1_56_0 recognizer.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/utility.hpp>
#include <boost/range.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <list>
#include <functional>
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
  return [] (std::list<A> const& cs) -> remaining<A>
    {
      return remains<A> (cs.begin (), cs.end ());
    };
}

//Given a predicate, 'recognizer_of_token' produces the recognizer
//associated with the elements that satisfy this predicate
template <class A, class F/*bool (char)*/>
recognizer<A> recognizer_of_token (F test)
{
  return
    [=] (std::list<A> const& cs) -> remaining<A>
    {
      if (cs.empty ())
        return recognition_fails<A> ();

      if (test (cs.front ()))
        return remains<A> (boost::next (cs.begin ()), cs.end ());

      return recognition_fails<A> ();
    };
}

//Recognizer disjunction
template <class A>
recognizer<A> compose_or (recognizer<A> p, recognizer<A> q)
{
  return [=] (std::list<A> const& toks) -> remaining<A>
    {
      remaining <A> res = p (toks);
      if (remains<A>* rem = boost::get<remains<A> > (&res)) return *rem;

      return q (toks);
    };
}

//Recognizer conjunction
template <class A>
recognizer<A> compose_and (recognizer<A> p, recognizer<A> q)
{
  return [=] (std::list<A> const& toks) -> remaining<A>
    {
      remaining <A> res = p (toks);
      if (remains<A>* rem = boost::get<remains<A> > (&res)) return q (rem->left);

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
template <class A>
recognizer<A> char_ (A c)
{
  return recognizer_of_token<A>([=](A x) -> bool { return c == x; });
}

//A function to produce a recognizer of a specific string
template <class C>
recognizer<typename boost::range_value<C>::type> string_ (C s)
{
  typedef typename boost::range_value<C>::type value;

  std::list <recognizer<value> > rs;

  typedef std::back_insert_iterator<std::list<recognizer<value> > > it_t;

  std::accumulate (
      boost::begin (s)
    , boost::end (s)
o    , std::back_inserter (rs)
    , [](it_t dst, value c) -> it_t 
      { *dst++ = char_ (c); return dst; });

  return std::accumulate (
      boost::next (rs.begin ())
    , rs.end ()
    , rs.front ()
    , [](recognizer<value> acc, recognizer<value> r) -> recognizer<value>
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
  return boost::apply_visitor ( accept_visitor<A> (), r);
}

//Test if the provided recognizer can recognize the given string
bool parse (recognizer<char> parser, std::string const& s)
{
  return accept (parser (std::list<char> (s.begin (), s.end ())));
}

int main ()
{
  //char_
  BOOST_TEST( parse (char_ ('a'), "a") );
  BOOST_TEST( !parse (char_ ('b'), "a") );
  BOOST_TEST( parse (char_ ('b'), "b") );

  //'*'
  BOOST_TEST( parse (zero_or_more (char_ ('a')), "aa") );
  BOOST_TEST( parse (zero_or_more (char_ ('a')), "") );
  BOOST_TEST( !parse (zero_or_more (char_ ('a')), "ab") );

  //string_
  BOOST_TEST (parse (string_ (std::string ("foo")), "foo") );
  BOOST_TEST (!parse (string_ (std::string ("foo")), "bar") );
  
  return boost::report_errors ();
}
