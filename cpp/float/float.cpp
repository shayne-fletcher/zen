//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x46
//cl /Fefloat.exe /Zi /MDd /EHsc /I d:/boost_1_56_0 float.cpp

#include <string>
#include <iostream>
#include <functional>
#include <algorithm>

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>

struct _neg; 
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
std::ostream& operator << (std::ostream& os, _neg const& e);
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
    , boost::recursive_wrapper<_neg>
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
struct _neg { expression f; };
struct _fix { std::string d; expression f; };
struct _add { expression lhs; expression rhs; };
struct _sub { expression lhs; expression rhs; };
struct _mul { expression lhs; expression rhs; };
struct _div { expression lhs; expression rhs; };
struct _max { expression lhs; expression rhs; };
struct _min { expression lhs; expression rhs; };

std::ostream& operator << (std::ostream& os, _const const& e) { return os << e.f; }
std::ostream& operator << (std::ostream& os, _market const& e) {return os << "market(\"" << e.tag << "\")"; }
std::ostream& operator << (std::ostream& os, _neg const& e) { return os << "-(" << e.f << ")"; }
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
    expression operator ()(_neg const& x) const { _neg res; res.f = fix (d, x.f); return res; }
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

int main () {

  expression asset=market ("IBM");
  expression number_of_shares = cst (100);
  expression payoff = number_of_shares * fix ("2016-09-18", max(cst (0.5), min (asset / fix ("2015-09-18", asset) - cst (1.0), cst (0.2))));

  std::cout << payoff << std::endl;

  return 0;
}
