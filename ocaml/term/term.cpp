//"c:/program files (x86)/Microsoft Visual Studio 10.0/vc/vcvarsall.bat" x64
//cl /Feterm.exe /EHsc /I d:/boost_1_56_0 term.cpp

#include <boost/variant.hpp>
#include <boost/range.hpp>
#include <boost/assign/list_of.hpp>

#include <iostream>
#include <list>
#include <string>
#include <numeric>
#include <cstring>

/*
let concat sep l =
  match l with
    [] -> ""
  | hd :: tl ->
      let num = ref 0 and len = ref 0 in
      List.iter (fun s -> incr num; len := !len + length s) l;
      let r = create (!len + length sep * (!num - 1)) in
      unsafe_blit hd 0 r 0 (length hd);
      let pos = ref(length hd) in
      List.iter
        (fun s ->
          unsafe_blit sep 0 r !pos (length sep);
          pos := !pos + length sep;
          unsafe_blit s 0 r !pos (length s);
          pos := !pos + length s)
        tl;
      r
 */

namespace string_util /*In honor of Stefano :)*/ 
{
  template <class RgT>
  std::string concat (std::string const& sep, RgT lst)
  {
    using boost::empty;
    using boost::begin;
    using boost::end;

    if (boost::empty (lst))
      return std::string ("");

    std::size_t num = 0, len = 0;
    std::accumulate (
      boost::begin (lst), boost::end (lst), 0,
      [&](int _, std::string const& s) -> 
      int { ++num, len += s.size(); return _; } );
    std::string r(len + sep.size () * (num - 1), '\0');
    std::string const& hd = *(boost::begin (lst));
    std::memcpy ((void*)(r.data ()), (void*)(hd.data ()), hd.size());
    std::size_t pos = hd.size();
    std::accumulate (
      boost::next (boost::begin (lst)), boost::end (lst), 0,
      [&](int _, std::string const& s) -> 
      int {
        std::memcpy((void*)(r.data ()+pos),(void*)(sep.data()),sep.size ());
        pos += sep.size ();
        std::memcpy ((void*)(r.data()+pos),(void*)(s.data()),s.size ());
        pos += s.size ();
        return _; });
  
  return r;
}
}//namespace<string_util>

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
  typedef boost::variant <term <A, B>, var<B> > type;
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
  A a, std::list<typename make_tree<A, B>::type> const& c, B const*)
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

namespace {
  struct string_of_term_visitor 
  {
    typedef std::string result_type;

    template <class B>
    std::string operator ()(var<B> const& v) const 
    { 
      return v.tag; 
    }
    template <class A, class B>
    std::string operator()(term<A, B> const& t) const 
    {
      return "(" + std::accumulate (
         t.children.begin (), t.children.end(), t.a, 
         [](std::string const& acc, typename make_tree<A, B>::type const& x) {
           return acc + boost::apply_visitor(string_of_term_visitor(), x) + ",";
         }) + ")";
    }
  };

}//namespace<anonymous>

template <class T> 
inline std::string string_of_term (T const& t)
{
  return boost::apply_visitor (string_of_term_visitor(), t);
}

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

  std::cout << string_of_term (t) << std::endl;

  std::list <std::string> l = boost::assign::list_of ("foo")("bar")("baz");

  std::string r = string_util::concat (std::string (","), l);
  std::cout << r << std::endl;
  
  return 0;
}
