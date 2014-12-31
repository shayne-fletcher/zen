//g++ -std=c++11 -I ~/project/boost_1_55_0 -o analyzer analyzer.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/optional.hpp>
#include <boost/utility.hpp>

#include <utility>
#include <list>
#include <functional>
#include <cassert>
#include <type_traits>
#include <cstdlib>

//Analysis succeeded
template <class A, class B> 
struct returns {
  std::pair<B, std::list<A>> result;
  template <class ItT> 
    returns (B const& b, ItT begin, ItT end) { 
    result.first = b; result.second.assign (begin, end); }
};

//Analysis fails
template <class A, class B>
struct analyze_fails {};

//The result of an analysis
template <class A, class B> using parsed = 
  boost::variant<returns<A, B>, analyze_fails<A, B> >;

//A parser is a function from a 'list<A>' to a value of 'returns<A,
//B>'
template <class A, class B> using parser =
  std::function<parsed<A, B> (std::list<A> const&)>;

//The parser that recognizes the empty string
template <class A, class B>
parser<A, B> empty (B v) {
  return [=] (std::list<A> const& ts) -> parsed<A, B> {
      return returns<A, B> (v, ts.begin (), ts.end ());
    };
}

//Given a predicate, 'token' produces the parser associated with
//the elements that satisfy this predicate
template <class A, class B, class F /*optional<B>(A)*/>
parser<A, B> token (F test) {
  return [=] (std::list<A> const& ts) -> parsed<A, B> {
      if (ts.empty ())
        return analyze_fails<A, B> ();
      if (boost::optional <B> b = test (ts.front ()))
        return returns <A, B>(*b, boost::next (ts.begin ()), ts.end ());
      return analyze_fails<A, B> ();
    };
}

//An parser that accepts a given symbol
template <class A>
parser<A, A> char_ (A c) {
  return token<A, A> (
         [=](A ch) -> boost::optional<A> {
           if (ch == c) 
             return boost::optional<A> (c);
           return boost::optional<A> ();
      });
}

//'>=' changes a return value produced by an parser in order to
//re-organize such values into data structures
template <class A, class B, class F/*C(B)*/>
parser <A, typename std::result_of<F (B)>::type> operator >= (parser<A, B> p,  F f) {
  typedef typename std::result_of<F (B)>::type C;
  return [=] (std::list<A> const& toks) -> parsed<A, C> {
      parsed<A, B> res = p (toks);
      if (returns <A, B>* r = boost::get <returns <A, B> >(&res))
          return returns<A, C> (
             f (r->result.first)
           , r->result.second.begin (), r->result.second.end ());
      return analyze_fails<A, C> ();
    };
}

//'|', parser disjunction
template <class A, class B>
parser<A, B> operator | (parser<A, B> const& p1, parser<A, B> const& p2) {
  return [=](std::list<A> const& toks) -> parsed<A, B> {
      parsed<A, B> res = p1 (toks);
      if (analyze_fails<A, B>* r = boost::get <analyze_fails<A, B> >(&res))
        return p2 (toks);
      return res;
    };
}

//'>>', parser conjunction
template <class A, class B, class C>
parser<A, std::pair<B, C> > operator >> (parser<A, B> const& p1, parser<A, C> const& p2) {
  return [=](std::list<A> const& toks) -> parsed<A, std::pair<B, C> > {
      parsed<A, B> res1 = p1 (toks);
      if (returns<A, B>* r1 = boost::get<returns<A, B> >(&res1)) {
          parsed<A, C> res2 = p2 (r1->result.second);
          if(returns<A, C>* r2 = boost::get<returns<A, C> >(&res2))
              return returns<A, std::pair<B, C> > (
                std::make_pair (r1->result.first, r2->result.first)
              , r2->result.second.begin (), r2->result.second.end ());
          return analyze_fails<A, std::pair<B, C> > ();
        }
      return analyze_fails<A, std::pair<B, C> > ();
    };
}

//Kleene star iterator
template <class A, class B>
parser<A, std::list<B>> operator * (parser<A, B> const& p) {
  return [=] (std::list<A> const& toks) -> parsed<A, std::list<B>>{
      return (((p >> *(p)) >= 
        [](std::pair<B, std::list<B> > r) -> std::list<B> {
          std::list<B> l(r.second);
          l.push_front(r.first);
          return l;
        }) | empty<A, std::list<B> > (std::list<B>())) (toks);
    };
}

//test if character is in a range
bool char_range (char c, std::list<std::pair<char, char>> l) {
  if (l.empty ()) return false;
  std::pair<char, char> r = l.front ();
  if (r.first <= c && c <= r.second) return true;
  l.pop_front ();
  return char_range (c, l);
}

//test for digit
bool is_digit (char c) {
  return char_range (c, std::list<std::pair<char, char>>{std::make_pair('0', '9')});
}

// digit := '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
parser<char, char> digit = 
  token<char, char> (
     [](char c) -> boost::optional<char> {
       if (is_digit (c)) 
         return boost::optional<char> (c); 
       else return boost::optional<char> ();
     });

//digits := digit digit*
parser <char, std::list<char>> digits =
  ((digit >> *digit)) >= 
  [](std::pair<char, std::list<char>> res) -> std::list<char> {
    res.second.push_front (res.first);
    return res.second;
};

//optsign := '+' | '-' | epsilon
parser<char, std::list<char>> optsign =
  (token<char, std::list<char>> (
    [](char c) -> boost::optional<std::list<char>> {
      if (c == '-' || c == '+') {
          std::list<char> cs;
          cs.push_front (c);
          return boost::optional<std::list<char>>(cs);
        }
      else
          return boost::optional<std::list<char>>();
    })) | empty<char, std::list<char>> (std::list<char>());

//optfrac := ('.' digit*)|epsilon
parser<char, std::list<char>> optfrac =
  (char_ ('.') >> *digit >= 
  [](std::pair<char, std::list<char>> res) -> std::list<char>{
    res.second.push_front (res.first);
    return res.second;
  }) | empty<char, std::list<char>> (std::list<char>());

//optexp := (('e'|'E') optsign digits)|epsilon
parser<char, std::list<char>> optexp =
  (((((char_ ('e') | char_ ('E')) >> optsign) >= 
  [](std::pair<char, std::list<char>> res) -> std::list<char> {
    res.second.push_front (res.first);
    return res.second;
  }) >> digits) >= 
  [](std::pair<std::list<char>, std::list<char>> res) -> std::list<char> {
    res.first.splice(res.first.end(), res.second);
    return res.first;
  }) | empty<char, std::list<char>>(std::list<char>());

//float_ := digits optfrac optexp
parser<char, double> float_ =
  (digits >> optfrac >> optexp) >=
  [](std::pair<std::pair<std::list<char>, std::list<char>>, std::list<char>> res) -> double
  {
    std::list<char>& csi = res.first.first, csf = res.first.second, cse = res.second;
    std::list<char> t(csi); t.splice(t.end(), csf); t.splice(t.end(), cse);
    return std::atof((std::string (t.begin (), t.end ())).c_str());
  };

//Parse a string
template <class P>
typename std::result_of<P (std::list<char> const&)>::type 
parse (P const& parser, std::string const& s) {
  return parser (std::list<char>(s.begin (), s.end ()));
}

//A function to extract the result of a parse
template <class A, class B>
struct accept_visitor {
  typedef B result_type;
  B operator () (returns<A, B> const& r) const {
    if (r.result.second.empty ()) return r.result.first;
    throw std::runtime_error ("Couldn't consume all input");
  }
  B operator ()(analyze_fails<A, B> const&) const {
    throw std::runtime_error ("Failed");
  }
};
template <class A, class B>
B accept (parsed<A, B> const& res) {
  return boost::apply_visitor (accept_visitor<A, B> (), res);
}

//test
int main () {
  try {
      std::cout << accept (parse (float_, "123.e-5")) << std::endl;
    }
  catch (std::runtime_error const& e) {
      std::cerr << e.what() << '\n';
    }

 return 0;
}
