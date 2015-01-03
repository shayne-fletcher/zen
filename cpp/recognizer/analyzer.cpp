//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl /Feanalyzer.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 analyzer.cpp

//g++ -std=c++11 -I ~/project/boost_1_55_0 -o analyzer analyzer.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/optional.hpp>
#include <boost/utility.hpp>
#include <boost/range.hpp>

#include <utility>
#include <list>
#include <functional>
#include <type_traits>
#include <cstdlib>
#include <sstream>
#include <cstring>

//Parse success
template <class A, class B> 
struct returns {
  std::pair<B, std::list<A>> result;
  template <class ItT> 
    returns (B const& b, ItT begin, ItT end) { 
    result.first = b; result.second.assign (begin, end); }
};

//Parse failed
template <class A, class B>
struct parse_fails {};

//The result of a parse
template <class A, class B> using parsed = 
  boost::variant<returns<A, B>, parse_fails<A, B> >;

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
        return parse_fails<A, B> ();
      if (boost::optional <B> b = test (ts.front ()))
        return returns <A, B>(*b, boost::next (ts.begin ()), ts.end ());
      return parse_fails<A, B> ();
    };
}

//A parser that accepts a given symbol
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
      return parse_fails<A, C> ();
    };
}

//'|', parser disjunction
template <class A, class B>
parser<A, B> operator | (parser<A, B> const& p1, parser<A, B> const& p2) {
  return [=](std::list<A> const& toks) -> parsed<A, B> {
      parsed<A, B> res = p1 (toks);
      if (parse_fails<A, B>* r = boost::get <parse_fails<A, B> >(&res))
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
          return parse_fails<A, std::pair<B, C> > ();
        }
      return parse_fails<A, std::pair<B, C> > ();
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

//Test if character is in a range
bool char_range (char c, std::list<std::pair<char, char>> l) {
  if (l.empty ()) return false;
  std::pair<char, char> r = l.front ();
  if (r.first <= c && c <= r.second) return true;
  l.pop_front ();
  return char_range (c, l);
}

//Test for digit
bool is_digit (char c) {
  return char_range (c, std::list<std::pair<char, char>>{std::make_pair('0', '9')});
}

//Test for letter
bool is_letter (char c) {
  return char_range (c, 
    std::list<std::pair<char, char>>{std::make_pair ('a', 'z'), std::make_pair ('A', 'Z')});
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

//Tokens

struct T_num { double val; };
struct T_ident { std::string val; };
struct T_lparen {};
struct T_rparen {};
struct T_plus {};
struct T_minus {};
struct T_star {};
struct T_slash {};
typedef boost::variant <
  T_num, 
  T_ident, 
  T_lparen, T_rparen, 
  T_plus, T_minus, T_star, T_slash> token_t;

struct string_of_token_visitor {
  typedef std::string result_type;

  std::string operator()(T_lparen) const { return "T_lparen"; }
  std::string operator()(T_rparen) const { return "T_rparen"; }
  std::string operator()(T_plus) const { return "T_plus"; }
  std::string operator()(T_minus) const { return "T_minus"; }
  std::string operator()(T_star) const { return "T_star"; }
  std::string operator()(T_slash) const { return "T_slash"; }
  std::string operator ()(T_num tok) const { 
    std::ostringstream os;
    os << "T_num " << tok.val;
    return os.str();
  }
  std::string operator ()(T_ident tok) const {
    std::ostringstream os;
    os << "T_ident \"" << tok.val << "\"";
    return os.str();
  }
};

std::string string_of_token (token_t const& tok) {
  return boost::apply_visitor (string_of_token_visitor (), tok);
}

//Lexical parser

parser<char, token_t> number =
  (digits >> optfrac >> optexp) >=
  [](std::pair<std::pair<std::list<char>, std::list<char>>, std::list<char>> res) -> token_t {
    std::list<char>& csi = res.first.first, csf = res.first.second, cse = res.second;
    std::list<char> t(csi); t.splice(t.end(), csf); t.splice(t.end(), cse);
    T_num tok; tok.val = std::atof((std::string (t.begin (), t.end ())).c_str());
    return tok;
  };

//letter := ['a'..'z'] | ['A'..'Z']
parser<char, char> letter =
  token<char, char> (
         [](char c) -> boost::optional<char> {
           if (is_letter (c)) 
             return boost::optional<char>(c);
           return boost::optional<char>();
         });

//identifer := letter *letter
parser<char, token_t> identifier =
  (letter >> *(letter)) >=
    [](std::pair<char, std::list<char>> res) -> token_t {
      res.second.push_front (res.first);
      std::string s(res.second.begin (), res.second.end ());
      T_ident tok; tok.val = s;
      return tok;
    };

//operator := '+' | '-' | '*' | '/'
parser<char, token_t> operator_ =
  token<char, token_t> (
         [](char c) -> boost::optional<token_t> {
           switch (c) {
           case '+' : return boost::optional<token_t>(T_plus ());
           case '-' : return boost::optional<token_t>(T_minus ());
           case '*' : return boost::optional<token_t>(T_star ());
           case '/' : return boost::optional<token_t>(T_slash ());
           default: return boost::optional<token_t>();
           }
         }
       );

//paren := '(' | ')'
parser<char, token_t> paren = 
  token<char, token_t> (
         [](char c) -> boost::optional<token_t> {
           switch (c) {
           case '(' : return boost::optional<token_t>(T_lparen ());
           case ')' : return boost::optional<token_t>(T_rparen ());
           default: return boost::optional<token_t>();
           }
         }
        );

struct unit_t {};//The single point type
unit_t const unit;

//space := ' '|'\t'|'\r'|'\n'
parser <char, unit_t> space =
  token<char, unit_t> (
         [](char c) -> boost::optional<unit_t> {
           switch (c) {
             case ' ':
             case '\t':
             case '\r':
             case '\n':
               return boost::optional<unit_t>(unit);
             default : return boost::optional<unit_t>();
             }
         }
  );

//spaces := space *space
parser<char, unit_t> spaces =
  ((space >> *space) >= 
  [](std::pair<unit_t, std::list<unit_t>>) -> unit_t { 
    return unit; }) 
  | empty<char, unit_t>(unit);

//lex := spaces *((identifier | number | operator | paren) spaces)
parser<char, std::list<token_t>> lex =
  spaces >> 
  *(((identifier|number|operator_|paren) >> spaces)>= 
    [](std::pair<token_t, unit_t> res) -> token_t { 
      return res.first; }) 
  >= [](std::pair<unit_t, std::list<token_t>> res) -> std::list<token_t> { 
  return res.second; };

//Expressions

struct E_constant;
struct E_variable;
struct E_addition;
struct E_subtraction;
struct E_multiplication;
struct E_division;
typedef boost::variant<
    E_constant
  , E_variable
  , boost::recursive_wrapper<E_addition>
  , boost::recursive_wrapper<E_subtraction>
  , boost::recursive_wrapper<E_multiplication>
  , boost::recursive_wrapper<E_division>
  > expression_t;
struct E_constant { double val; };
struct E_variable { std::string val; };
struct E_addition { expression_t left; expression_t right; };
struct E_subtraction { expression_t left; expression_t right; };
struct E_multiplication { expression_t left; expression_t right; };
struct E_division { expression_t left; expression_t right; };

typedef std::function<expression_t(expression_t, expression_t)> op_t;

std::string string_of_expression (expression_t const& e); //fwd. decl.

struct string_of_expression_visitor {
  typedef std::string result_type;

  std::string operator ()(E_constant const& e) const {
    std::ostringstream os;
    os << "E_constant (" << e.val<< ")";
    return os.str();
  }
  std::string operator ()(E_variable const& e) const {
    std::ostringstream os;
    os << "E_variable(" << e.val<< ")";
    return os.str();
  }
  std::string operator ()(E_addition const& e) const {
    std::ostringstream os;
    os << "E_addition(" 
       << string_of_expression (e.left) 
       << "," 
       << string_of_expression (e.right) 
       << ")";
    return os.str();
  }
  std::string operator ()(E_subtraction const& e) const {
    std::ostringstream os;
    os << "E_subtraction(" 
       << string_of_expression (e.left) 
       << "," 
       << string_of_expression (e.right) 
       << ")";
    return os.str();
  }
  std::string operator ()(E_multiplication const& e) const {
    std::ostringstream os;
    os << "E_multiplication(" 
       << string_of_expression (e.left) 
       << "," 
       << string_of_expression (e.right) 
       << ")";
    return os.str();
  }
  std::string operator ()(E_division const& e) const {
    std::ostringstream os;
    os << "E_division(" 
       << string_of_expression (e.left) 
       << "," 
       << string_of_expression (e.right) 
       << ")";
    return os.str();
  }
};

std::string string_of_expression (expression_t const& e) {
  return boost::apply_visitor (string_of_expression_visitor (), e);
}

template <class A, class B, class F>
typename std::result_of<F (B)>::type operator >>= (parser<A, B> p1, F p2)
{
  typedef typename std::result_of<F (B)>::type parser_t;
  typedef typename std::result_of<parser_t(std::list<A> const&)>::type parsed_t;

  return [=](std::list<A> const& toks) -> parsed_t
    {
      parsed<A, B> res = p1 (toks);
      if(returns<A, B>* r = boost::get<returns<A, B>>(&res))
        {
          B b = r-> result.first;
          parser<A, B> f = p2 (b);
          return f (r->result.second);
        }
      return parse_fails<A, B> ();
    };
}

template <class A, class B>
struct sequence_
{
  parser<A, B> term_;
  parser<A, std::function<B(B, B)>> op__;
  sequence_(parser<A, B> term, parser<A, std::function<B(B, B)>> op_)
    : term_(term), op__(op_)
  {}

  parser<A, B> operator ()(B t1) const
  {
      return
      (((op__ >> term_) >= 
        [=](std::pair<std::function<B(B,B)>,B> res) -> B {
          return res.first (t1, res.second); }
        ) >>= sequence_<A, B>(term_, op__))| empty <A, B>(t1);
  }
};

template <class A, class B>
parser<A, B> left_assoc (parser<A, B> term, parser<A, std::function<B(B, B)>> op_)
{
  sequence_<A, B> seq (term, op_);
  parser<A, B> p = (term >>= seq);
  return p;
}

parser<token_t, expression_t> num = 
  token<token_t, expression_t> (
    [](token_t tok) -> boost::optional<expression_t> {
      if (T_num* t = boost::get<T_num>(&tok)) {
          E_constant e; e.val = t->val;
          return boost::optional<expression_t>(e);
        }
      return boost::optional<expression_t>();
    }
   );

parser<token_t, expression_t> ident =
  token<token_t, expression_t> (
    [](token_t tok) -> boost::optional<expression_t> {
      if (T_ident* t = boost::get<T_ident>(&tok)) {
          E_variable e; e.val = t->val;
          return boost::optional<expression_t>(e);
        }
      return boost::optional<expression_t>();
    }
   );

parser<token_t, std::function<expression_t(expression_t, expression_t)>> addop =
  token<token_t, op_t>(
    [](token_t tok) -> boost::optional<op_t> {
      if (T_plus* t = boost::get<T_plus>(&tok)) {
          op_t add = [](expression_t e1, expression_t e2) -> expression_t {
            E_addition e; e.left = e1; e.right = e2;
            return e;
          };
          return boost::optional<op_t>(add);
        }
      if (T_minus* t = boost::get<T_minus>(&tok)) {
          op_t sub = [](expression_t e1, expression_t e2) -> expression_t {
            E_subtraction e; e.left = e1; e.right = e2;
            return e;
          };
          return boost::optional<op_t>(sub);
        }
      return boost::optional<op_t>();
    }
  );

parser<token_t, std::function<expression_t(expression_t, expression_t)>> mulop =
  token<token_t, op_t>(
    [](token_t tok) -> boost::optional<op_t> {
      if (T_star* t = boost::get<T_star>(&tok)) {
          op_t mul = [](expression_t e1, expression_t e2) -> expression_t {
            E_multiplication e; e.left = e1; e.right = e2;
            return e;
          };
          return boost::optional<op_t>(mul);
        }
      if (T_slash* t = boost::get<T_slash>(&tok)) {
        op_t div = [](expression_t e1, expression_t e2) -> expression_t {
          E_division e; e.left = e1; e.right = e2;
          return e;
        };
        return boost::optional<op_t>(div);
      }
      return boost::optional<op_t>();
    }
  );

parser<token_t, unit_t> open_paren =
  token<token_t, unit_t>(
    [](token_t tok) -> boost::optional<unit_t> {
      if(boost::get<T_lparen>(&tok))
        return boost::optional<unit_t>(unit);
      return boost::optional<unit_t>();
    }
  );

parser<token_t, unit_t> close_paren =
  token<token_t, unit_t>(
    [](token_t tok) -> boost::optional<unit_t> {
      if(boost::get<T_rparen>(&tok))
        return boost::optional<unit_t>(unit);
      return boost::optional<unit_t>();
    }
  );

/*
(*
expr :=
  | term (['+'|'-'] term)*
  ;
term :=
  | fact (['*'|'/'] fact)*
  ;
fact :=
  | num
  | identifier
  | '( expr ')
 *)
 */

parsed<token_t, expression_t> expr (std::list<token_t> const& toks);
parsed<token_t, expression_t> term (std::list<token_t> const& toks);
parsed<token_t, expression_t> fact (std::list<token_t> const& toks);

parsed<token_t, expression_t> expr (std::list<token_t> const& toks) {
  return (left_assoc (parser<token_t, expression_t>(term), addop)) (toks);
}

parsed<token_t, expression_t> term (std::list<token_t> const& toks) {
  return (left_assoc (parser<token_t, expression_t>(fact), mulop)) (toks);
}

parsed<token_t, expression_t> fact (std::list<token_t> const& toks) {
  return
    (num | ident | 
     ((open_paren >> parser<token_t, expression_t>(expr) >> close_paren) >=
      [=](std::pair<std::pair<unit_t, expression_t>, unit_t> res) -> expression_t {
       return res.first.second;
      }))(toks);
}

parsed<token_t, expression_t> analyze_expr (std::list<token_t> const& toks)
{
  return expr (toks);
}

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
  B operator ()(parse_fails<A, B> const&) const {
    throw std::runtime_error ("Failed");
  }
};
template <class A, class B>
B accept (parsed<A, B> const& res) {
  return boost::apply_visitor (accept_visitor<A, B> (), res);
}

//Test
int main () {

  try {
    std::cout << 
      string_of_expression (
         accept (analyze_expr (
            accept (parse (lex, "1 + 2 + 3 - 4 - x*6"))))) 
              << std::endl;
  }
  catch (std::runtime_error const& e) {
      std::cerr << e.what() << '\n';
    }

 return 0;
}

