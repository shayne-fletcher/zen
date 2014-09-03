//"c:/program files (x86)/Microsoft Visual Studio 10.0/vc/vcvarsall.bat" x64
//cl /Feterm.exe /EHsc /I d:/boost_1_55_0 term.cpp

#include <list>

#include <boost/variant.hpp>

/*
  type ('a, 'b) term =
    | Term of 'a * ('a, 'b) term list
    | Var of 'b
*/

template <class A, class B> struct term;
template <class B> struct var;

template <class A, class B>
struct make_tree
{
  typedef boost::variant <
    boost::recursive_wrapper<term <A, B> >,
    boost::recursive_wrapper<var<B> > >   type;
};

template <class A, class B>
struct term
{
  typedef typename make_tree <A, B>::type tree;
  A a;
  std::list <tree> children;
  term (A a
    , std::list<tree> const& children)
    :       a (a), children (children)
  {}
};

template <class A, class B> 
inline term <A, B> make_term (
  A a, std::list<typename make_tree<A, B>::type> const& c, B*)
{
  return term<A, B> (a, c);
}

template <class B> 
struct var
{
  B tag;
  var (B tag) : tag (tag) {}
};

template <class B>
inline var<B> make_var (B tag) { return var<B> (tag); }

int main ()
{
  typedef make_tree<std::string, std::string>::type tree;
  typedef std::list<tree> term_list;
  std::string const* tag_str=(std::string const*)0L;

  // a(b(), c)
  term_list terms;
  terms.push_back (make_term(std::string("b"), term_list (), tag_str));
  terms.push_back (make_var(std::string("c")));
  tree t = make_term(std::string("a"), terms, tag_str);
  
  return 0;
}
