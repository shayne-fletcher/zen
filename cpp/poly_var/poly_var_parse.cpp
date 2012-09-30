/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//poly_var_parse.cpp

#include <cpp/poly_var/poly_var.h>

#include <boost/config/warning_disable.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/foreach.hpp>
#include <boost/range/iterator_range.hpp>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>

#include <boost/detail/lightweight_test.hpp>

#include <iostream>
#include <string>
#include <cassert>
#include <algorithm>

std::string string_of_poly_var(poly_var const& p);
std::string poly_var_array_to_string(poly_var const& p);

//Examples of the chosen concrete syntax.
//
//  Nil
//  W 1
//  Num 1.0
//  Str "text"
//  Bool true (false)
//  Array ([e1;e2;...],m,n)

//The next two functions are mutually recursive.

std::string string_of_poly_var(poly_var const& p);//Fwd. decl

std::string poly_var_array_to_string(poly_var const& p)
{
  std::string res="Array ([";
  std::size_t const rows=p.val.array.rows, columns=p.val.array.columns;
  poly_var const* first=(poly_var const*)p.val.array.data;

  bool init=true;
  BOOST_FOREACH(poly_var const& a, 
    boost::make_iterator_range(first, first+rows*columns))
    {
      if(init)
  	init=false;
      else
  	res+=";";

      res += string_of_poly_var(a);
    }
  res += "],"+boost::lexical_cast<std::string>(rows)+
    ","+boost::lexical_cast<std::string>(columns)+")";

  return res;
}

// string_of_poly_var : poly_var -> string

std::string string_of_poly_var(poly_var const& p)
{
  switch(p.type)
    {
    case pv_type_int:
      return std::string("W ")+boost::lexical_cast<std::string>(p.val.w);
    case pv_type_num:
      return std::string("Num ")+ boost::lexical_cast<std::string>(p.val.num);
    case pv_type_str:
	return std::string("Str \"")+p.val.str+"\"";
    case pv_type_bool:
      return std::string("Bool ")+(p.val.bool_?"true":"false");
    case pv_type_missing:
    case pv_type_nil:
      return std::string("Nil");
    case pv_type_multi:
      return poly_var_array_to_string(p);
    default:
      assert(false);
    }

  return "";//Never get here.
}

//Tell Spirit poly_var is not an STL container.
namespace boost { namespace spirit { namespace traits {
    template<> struct is_container<poly_var> : boost::mpl::false_ {};
}}} //namespace boost::spirit::traits

namespace detail {//Helpers for poly_var parsing.

poly_var make_poly_var_array(
  boost::fusion::vector<std::vector<poly_var>, int, int> const& args)
{ 
  std::vector<poly_var> const& t=boost::fusion::at_c<0>(args);
  std::size_t rows = boost::fusion::at_c<1>(args);
  std::size_t columns = boost::fusion::at_c<2>(args);

  poly_var res(rows,columns);
  std::copy(t.begin(),t.end(),res.val.array.data);

  return res;
}

}//namespace detail

//struct poly_var_parser

template <class ItT>
struct poly_var_parser 
  : boost::spirit::qi::grammar<ItT, poly_var(), boost::spirit::ascii::space_type>
{
  poly_var_parser() : 
    poly_var_parser::base_type(start)
  {
    using boost::spirit::qi::int_;
    using boost::spirit::lit;
    using boost::spirit::ascii::char_;
    using boost::spirit::ascii::string;
    using boost::spirit::true_;
    using boost::spirit::false_;
    using boost::spirit::bool_;
    using boost::spirit::qi::_val;
    using boost::spirit::qi::_1;
    using boost::spirit::qi::lexeme;

    start = 
        Nil
      | W
      | Num 
      | Bool
      | Str
      | Array
      ;

    Nil = string("Nil")[_val = poly_var()] ;
    W = "W" >> int_  ;
    Num = "Num" >> strict_double ;
    Bool = "Bool" >> bool_ ;
    Str =  "Str" >> quoted_text ;
    Array = "Array" >> array[_val=
       boost::phoenix::bind(detail::make_poly_var_array, _1)] ;
    array =  
      lit('(') >> 
      lit('[') >> list >> lit(']') >> 
      lit(',') >> int_ >> lit(',') >> int_ >> 
      lit(')')
      ;
    list = (start % ';') ;
    quoted_text = "\"" >> cdata >> "\"" ;
    cdata %= lexeme[ +(char_ - "\"") ] ;
  }

  typedef boost::spirit::ascii::space_type skipper;
  boost::spirit::qi::real_parser<double, 
    boost::spirit::qi::strict_real_policies<double> > strict_double;

  boost::spirit::qi::rule<ItT, poly_var(), skipper> start;
  boost::spirit::qi::rule<ItT, poly_var(), skipper> Nil;
  boost::spirit::qi::rule<ItT, poly_var(), skipper> W;
  boost::spirit::qi::rule<ItT, poly_var(), skipper> Num;
  boost::spirit::qi::rule<ItT, poly_var(), skipper> Bool;
  boost::spirit::qi::rule<ItT, poly_var(), skipper> Str;
  boost::spirit::qi::rule<ItT, poly_var(), skipper> Array;
  boost::spirit::qi::rule<ItT, poly_var(), skipper> quoted_text;

  boost::spirit::qi::rule<ItT, boost::fusion::vector<
    std::vector<poly_var>,int, int>(), skipper> array;
  boost::spirit::qi::rule<ItT, std::vector<poly_var>(), skipper> list;
  boost::spirit::qi::rule<ItT, std::string(), skipper> cdata;
};

bool poly_var_parse(std::string const& input, poly_var& result)
{
  using boost::spirit::qi::phrase_parse;
  using boost::spirit::ascii::space;
  typedef std::string::const_iterator iterator_type;
  typedef poly_var_parser<iterator_type> poly_var_parser_type;

  poly_var_parser_type g;
  iterator_type iter = input.begin(), end = input.end();
  bool r = phrase_parse(iter, end, g, space, result);
  
  return r && (iter==end);
}

//poly_var_of_string : string->poly_var

poly_var poly_var_of_string(std::string const& s)
{
  poly_var result;
  bool parsed = poly_var_parse(s, result);
  if(!parsed)
    throw bad_poly_var_cast();

  return result;
}

//Tests.

int main()
{
  try
    {
      poly_var const nil; //Nil
      poly_var const one(1); //W 1
      poly_var abc=poly_var(3, 1);//Array ([1;2;3],1,3)
      poly_var pi(3.1415926535897932384626433832795); //Num 3.14159
      poly_var greeting("Hello world!");//Str "Hello world!"
      abc(0, 0)=poly_var(1); abc(1, 0)=poly_var(2); abc(2, 0)=poly_var(3); 

      //poly_var -> string

      std::string e = string_of_poly_var(nil);
      std::string f = string_of_poly_var(one);
      std::string g = string_of_poly_var(abc);
      std::string h = string_of_poly_var(greeting);
      std::string i = string_of_poly_var(pi);

      //string -> poly_var

      poly_var p=poly_var_of_string(e);//empty
      poly_var q=poly_var_of_string(f);//int
      poly_var r=poly_var_of_string(g);//array of int
      poly_var s=poly_var_of_string(h);//string
      poly_var t=poly_var_of_string(i);//double

      BOOST_TEST(nil.type==p.type&&nil.type==pv_type_missing);
      BOOST_TEST(one.type==q.type&&one.type==pv_type_int&&one.val.w==q.val.w);
      BOOST_TEST(abc.type==r.type&&abc.type==pv_type_multi&&
		 abc.val.array.rows==r.val.array.rows&&
		 abc.val.array.columns==r.val.array.columns);
      BOOST_TEST(abc.val.array.data->type==r.val.array.data->type&&
		 abc.val.array.data->type==pv_type_int&&
		 abc.val.array.data->val.w==r.val.array.data->val.w&&
		 abc.val.array.data->val.w==1);
      BOOST_TEST((abc.val.array.data+1)->type==(r.val.array.data+1)->type&&
		 (abc.val.array.data+1)->type==pv_type_int
		 &&
		 (abc.val.array.data+1)->val.w==(r.val.array.data+1)->val.w&&
		 (abc.val.array.data+1)->val.w==2);
      BOOST_TEST((abc.val.array.data+2)->type==(r.val.array.data+2)->type&&
		 (abc.val.array.data+2)->type==pv_type_int
		 &&
		 (abc.val.array.data+2)->val.w==(r.val.array.data+2)->val.w&&
		 (abc.val.array.data+2)->val.w==3);
      BOOST_TEST(t.type==pv_type_num&&std::abs(t.val.num - 3.14159)<=1.0e-4);
    }
  catch(std::exception const& e)
    {
      std::cerr << e.what() << '\n';
    }
  catch(...)
    {
      std::cerr << "unexpected exception\n";
    }

  return boost::report_errors();
}
