//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fefloat.exe /Zi /MDd /EHsc /I d:/boost_1_56_0 float.cpp

#include <string>
#include <iostream>
#include <functional>
#include <algorithm>
#include <numeric>
#include <iterator>
#include <vector>
#include <tuple>

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>

struct _fix;   
struct _add;   
struct _sub; 
struct _mul;   
struct _div;   
struct _max;   
struct _min;
struct _const; 
struct _market; 

std::ostream& operator << (std::ostream& os, _const const& e);
std::ostream& operator << (std::ostream& os, _market const& e);
std::ostream& operator << (std::ostream& os, _fix const& e);
std::ostream& operator << (std::ostream& os, _add const& e);
std::ostream& operator << (std::ostream& os, _sub const& e);
std::ostream& operator << (std::ostream& os, _mul const& e);
std::ostream& operator << (std::ostream& os, _div const& e);
std::ostream& operator << (std::ostream& os, _max const& e);
std::ostream& operator << (std::ostream& os, _min const& e);

using expression = 
  boost::variant<
      _const
    , _market 
    , boost::recursive_wrapper<_fix>
    , boost::recursive_wrapper<_add>
    , boost::recursive_wrapper<_sub>
    , boost::recursive_wrapper<_mul>
    , boost::recursive_wrapper<_div>
    , boost::recursive_wrapper<_min>
   , boost::recursive_wrapper<_max>  
>;

struct _const { double f; };
struct _market { std::string tag; };
struct _fix { std::string d; expression f; };
struct _add { expression lhs; expression rhs; };
struct _sub { expression lhs; expression rhs; };
struct _mul { expression lhs; expression rhs; };
struct _div { expression lhs; expression rhs; };
struct _max { expression lhs; expression rhs; };
struct _min { expression lhs; expression rhs; };

std::ostream& operator << (std::ostream& os, _const const& e) { return os << e.f; }
std::ostream& operator << (std::ostream& os, _market const& e) {return os << "market(\"" << e.tag << "\")"; }
std::ostream& operator << (std::ostream& os, _fix const& e) { return os << "fix(" << e.d << ", " << e.f << ")"; }
std::ostream& operator << (std::ostream& os, _add const& e) { return os << e.lhs << " + " << e.rhs; }
std::ostream& operator << (std::ostream& os, _sub const& e) { return os << e.lhs << " - " << e.rhs; }
std::ostream& operator << (std::ostream& os, _mul const& e) { return os << e.lhs << " * " << e.rhs; }
std::ostream& operator << (std::ostream& os, _div const& e) { return os << e.lhs << " / " << e.rhs; }
std::ostream& operator << (std::ostream& os, _max const& e) { return os << "max (" << e.lhs << ", " << e.rhs << ")"; }
std::ostream& operator << (std::ostream& os, _min const& e) { return os << "min (" << e.lhs << ", " << e.rhs << ")"; }

namespace detail {

  template <class Op>
  struct binop_visitor 
    : boost::static_visitor<expression>{
    typedef std::function<double(double, double)> op_t;
    op_t op;
    binop_visitor (op_t const& op) : op(op)
    {}
    expression operator()(_const lhs, _const rhs) const { 
      return _const {op (lhs.f, rhs.f)}; 
    }
    template <class T, class U> 
    expression operator ()(T const& x, U const& y) const { 
      Op res; res.lhs = x; res.rhs = y; return res; }
  };

}//namespace detail

expression operator + (expression const& lhs, expression const& rhs) {
  return boost::apply_visitor (detail::binop_visitor<_add> (
       [](double x, double y) ->double { return x + y; })
     , lhs, rhs);
}
expression operator - (expression const& lhs, expression const& rhs) {
  return boost::apply_visitor (detail::binop_visitor<_sub> (
       [](double x, double y) ->double { return x - y; })
     , lhs, rhs);
}
expression operator * (expression const& lhs, expression const& rhs) {
  return boost::apply_visitor (detail::binop_visitor<_mul> (
       [](double x, double y) -> double { return x * y; })
     , lhs, rhs);
}
expression operator / (expression const& lhs, expression const& rhs) {
  return boost::apply_visitor (detail::binop_visitor<_div> (
       [](double x, double y) -> double { return x / y; })
     , lhs, rhs);
}
expression max (expression const& lhs, expression const& rhs) {
  return boost::apply_visitor (detail::binop_visitor<_max> (
        [](double x, double y) -> double { return (std::max) (x, y); })
     , lhs, rhs);
}
expression min (expression const& lhs, expression const& rhs) {
  return boost::apply_visitor (
     detail::binop_visitor<_min> ([](double x, double y) -> double { return (std::min) (x, y); })
     , lhs, rhs);

}

expression fix (std::string const& d, expression const& x);

namespace detail {

  struct fix_visitor : boost::static_visitor<expression> {
    std::string d;
    fix_visitor (std::string const& d) : d(d){}
    expression operator ()(_const const& x) const { return x; }
    expression operator ()(_market const& x) const {  _fix res; res.d = d; res.f = x; return res; }
    expression operator ()(_fix const& x) const { return x; }
    expression operator ()(_add const& x) const { _add res; res.lhs = fix (d, x.lhs); res.rhs = fix (d, x.rhs); return res; }
    expression operator ()(_sub const& x) const { _sub res; res.lhs = fix (d, x.lhs); res.rhs = fix (d, x.rhs); return res; }
    expression operator ()(_mul const& x) const { _mul res; res.lhs = fix (d, x.lhs); res.rhs = fix (d, x.rhs); return res; }
    expression operator ()(_div const& x) const { _div res; res.lhs = fix (d, x.lhs); res.rhs = fix (d, x.rhs); return res; }
    expression operator ()(_max const& x) const { _max res; res.lhs = fix (d, x.lhs); res.rhs = fix (d, x.rhs); return res; }
    expression operator ()(_min const& x) const { _min res; res.lhs = fix (d, x.lhs); res.rhs = fix (d, x.rhs); return res; }
  };

}//namespace detail

expression cst (double f) { return _const{f}; }
expression market (std::string const& tag) { _market m; m.tag = tag; return m; }
expression fix (std::string const& d, expression const& x) {
  return boost::apply_visitor (detail::fix_visitor (d), x);
}

namespace detail {

template <class S, class D, class P>
D filter (S begin, S end, D dst, P p) {
  auto f = [&](D d, auto x) 
    { if (p (x)) return *d++ = x; return d; };

  return std::accumulate (begin, end, dst, f);
}

}//namespace detail

typedef std::tuple<std::string, std::string, double> fixing_t;
typedef std::vector<fixing_t> fixings_t;
expression simplify (fixings_t const& fs, expression const& x);

namespace detail {

  struct simplify_visitor : boost::static_visitor<expression> {
    fixings_t fs;
    simplify_visitor (fixings_t const& fs) : fs (fs) {}
    expression operator () (_const const& x) const { return x; }
    expression operator () (_market const& x) const { return x; }
    expression operator () (_fix const& x) const { 
      fixings_t buf;
      detail::filter (fs.begin (), fs.end ()
        , std::back_inserter(buf),[&](fixing_t const& f) { 
          _market const& m = boost::get<_market>(x.f);
          return m.tag == std::get<0>(f) && x.d == std::get<1>(f); });
      if (buf.empty ()) return x; return _const { std::get<2>(buf.front())}; }
    expression operator ()(_add const& x) const { expression lhs = simplify (fs, x.lhs), rhs = simplify (fs, x.rhs); return lhs + rhs; }
    expression operator ()(_sub const& x) const { expression lhs = simplify (fs, x.lhs), rhs = simplify (fs, x.rhs); return lhs - rhs; }
    expression operator ()(_mul const& x) const { expression lhs = simplify (fs, x.lhs), rhs = simplify (fs, x.rhs); return lhs * rhs; }
    expression operator ()(_div const& x) const { expression lhs = simplify (fs, x.lhs), rhs = simplify (fs, x.rhs); return lhs / rhs; }
    expression operator ()(_max const& x) const { expression lhs = simplify (fs, x.lhs), rhs = simplify (fs, x.rhs); return (max) (lhs, rhs); }
    expression operator ()(_min const& x) const { expression lhs = simplify (fs, x.lhs), rhs = simplify (fs, x.rhs); return (min) (lhs, rhs); }
  };

}//namespace detail

expression simplify (fixings_t const& fs, expression const& x) {
  return boost::apply_visitor (detail::simplify_visitor (fs), x); }

int main () {

  expression asset = market ("IBM");
  expression number_of_shares = cst (100);
  expression payoff = fix ("2016-09-18",
    number_of_shares * min (cst (0.3), max (asset / fix ("2015-09-18", asset) - cst (1.0), cst (0))));

  std::vector<fixing_t> fixings;
  fixings.push_back (std::make_tuple (std::string ("IBM"), std::string("2016-09-18"), 0.12));
  fixings.push_back (std::make_tuple (std::string ("IBM"), std::string("2015-09-18"), 0.10));
  
  std::cout << simplify (fixings, payoff) << std::endl;

  return 0;
}
