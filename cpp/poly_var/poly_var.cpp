#include <cpp/poly_var/poly_var.h>

#include <boost/foreach.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/preprocessor/iteration/local.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/numeric/conversion/cast.hpp>

#if defined(POLY_VAR_USE_LAMBDA) //Only if we have to.
#  include <boost/lambda/lambda.hpp>
#  include <boost/lambda/bind.hpp>
#else
//Boost.Phoenix preferred.
#  include <boost/spirit/include/phoenix_core.hpp>
#  include <boost/spirit/home/phoenix/bind/bind_function.hpp>
#  include <boost/spirit/home/phoenix/bind/bind_member_variable.hpp>
#  include <boost/spirit/home/phoenix/operator/comparison.hpp>
#endif//defined(POLY_VAR_USE_LAMBDA)

#include <memory>
#include <vector>
#include <iostream>
#include <string>
#include <algorithm>
#include <numeric>
#include <cstdarg>
#include <cstring>
#include <cstdlib>
#include <cassert>

struct poly_var_function
{
  std::size_t id;
  char const* name;
  std::size_t arity;
  void (*function)(void);
};

namespace //Home to the global function table.
{
  std::vector<poly_var_function> registered_functions;

}//namespace<anonymous>

extern "C" 
  void poly_var_api_register_functions(
    std::vector<poly_var_function>& functions);

/////////1////////2////////3////////4/////////5/////////6/////////7/////////8
// poly_var "raw" API

extern "C"
{

void poly_var_api_initialize()
{
  poly_var_api_register_functions(registered_functions);

  //Plug-in registration goes here.
}

void poly_var_api_uninitialize()
{
  //Nothing to do at this time.
}

void poly_var_init(poly_var_rep* p)
{
  std::memset(p, 0, sizeof(poly_var));
  p->type = pv_type_missing;
}

void poly_var_clear(poly_var_rep* p)
{
  if(p->type == pv_type_multi)
  {
    for(std::size_t i = 0; i < p->val.array.rows; ++i)
      for(std::size_t j = 0; j < p->val.array.columns; ++j)
    poly_var_clear(&(p->val.array.data[i*p->val.array.columns + j]));
    std::free(p->val.array.data);
  }
  else if(p->type == pv_type_str)
  {
    std::free(p->val.str);
  }

  p->type = pv_type_missing;
}

int poly_var_is_double(poly_var_rep_t const* p)
{
  return p->type == pv_type_num;
}

int poly_var_is_string(poly_var_rep_t const* p)
{
  return p->type == pv_type_str;
}

int poly_var_is_bool(poly_var_rep_t const* p)
{
  return p->type == pv_type_bool;
}

int poly_var_is_array(poly_var_rep_t const* p)
{
  return p->type == pv_type_multi;
}

int poly_var_is_missing(poly_var_rep_t const* p)
{
  return p->type == pv_type_missing;
}

int poly_var_is_nil(poly_var_rep_t const* p)
{
  return p->type == pv_type_nil;
}

int poly_var_is_int(poly_var_rep_t const* p)
{
  return p->type == pv_type_int;
}

void poly_var_copy(poly_var_rep* dst, poly_var_rep const* src)
{
  poly_var_init(dst);
  switch(src->type)
  {
  case pv_type_int:
    {
      dst->val.w = src->val.w;
    
      break;
    }
  case pv_type_num:
    {
      dst->val.num = src->val.num;
      
      break;
    }
  case pv_type_bool:
    {
      dst->val.bool_ = src->val.bool_;

      break;
    }
  case pv_type_str:
    {
      std::size_t n=std::strlen(src->val.str);
      char* str = (char*)std::malloc((n+1)*sizeof(char));
      std::strcpy(str, src->val.str);
      str[n]='\0';
      dst->val.str = str;

      break;
    }
  case pv_type_multi:
    {
      std::size_t n=src->val.array.rows*src->val.array.columns;
      dst->val.array.data = (poly_var*)std::malloc(n*sizeof(poly_var));
      for(std::size_t i = 0; i < n; ++i)
      {
	poly_var_rep* p=dst->val.array.data+i;
	poly_var_init(p);
	poly_var_copy(p, src->val.array.data+i);
      }
      dst->val.array.rows=src->val.array.rows;
      dst->val.array.columns=src->val.array.columns;

      break;
    }
  }

  dst->type = src->type;
}

//PP-meta code for poly_var_api_call_by_name() (to follow).

#define POLY_VAR_TEXT(z, n, text) text[n]

#define POLY_VAR_DISPATCH(n) \
  case n: \
  { \
    return poly_var_api_call_by_name(tag \
        , BOOST_PP_ENUM(n, POLY_VAR_TEXT, params)); \
  }\
  /**/

poly_var_rep_t* poly_var_api_call_by_name(char const* tag, size_t num_args, ...)
{
  std::va_list arguments;
  va_start(arguments, num_args);
  std::vector<poly_var const*> params;
  for(std::size_t k = 0; k < num_args; ++k)
  {
    params.push_back(va_arg(arguments, poly_var const*));
  }
  va_end(arguments);
  
  switch(num_args)
  {
  case 0:
    return poly_var_api_call_by_name(tag);
  case 1:
    return poly_var_api_call_by_name(tag, params[0]);

//Generate the remaining cases up to POLY_VAR_MAX_ARITY.

#define BOOST_PP_LOCAL_MACRO POLY_VAR_DISPATCH
#define BOOST_PP_LOCAL_LIMITS (2, POLY_VAR_API_MAX_ARITY)
#include BOOST_PP_LOCAL_ITERATE()    

  default:
    return (poly_var*)0L;
  }

  return (poly_var*)0L;
}

#undef POLY_VAR_DISPATCH
#undef POLY_VAR_TEXT

}//extern "C"

/////////1////////2////////3////////4/////////5/////////6/////////7/////////8
// struct poly_var implementation

poly_var::poly_var(int w)
{
  poly_var_init(this);
  this->val.w = w;
  this->type = pv_type_int;
}

poly_var::poly_var(double num)
{
  poly_var_init(this);
  this->val.num = num;
  this->type = pv_type_num;
}

poly_var::poly_var(bool bool_)
{
  poly_var_init(this);
  this->val.bool_ = bool_;
  this->type = pv_type_bool;
}

poly_var::poly_var(char const* s)
{
  poly_var_init(this);
  std::size_t n = std::strlen(s);
  char* str = (char*)std::malloc((n+1)*sizeof(char));
  std::strcpy(str, s);
  str[n] = '\0';

  this->val.str = str;
  this->type = pv_type_str;
}

poly_var::poly_var(std::string const& s)
{
  poly_var_init(this);
  std::size_t n = s.size();
  char* str = (char*)std::malloc((n+1)*sizeof(char));
  std::strcpy(str, s.c_str());
  str[n] = '\0';
  this->val.str = str;
  this->type = pv_type_str;
}

poly_var::poly_var(std::size_t rows, std::size_t cols)
{
  poly_var_init(this);
  this->val.array.data = (poly_var*)std::malloc(rows*cols*sizeof(poly_var));
  this->val.array.rows=rows;
  this->val.array.columns=cols;
  for(std::size_t i = 0; i < rows; ++i)
    for(std::size_t j = 0; j < cols; ++j)
      poly_var_init(&this->val.array.data[i*cols + j]);
  this->type = pv_type_multi;
}

#if !defined(BOOST_NO_RVALUE_REFERENCES)

poly_var::poly_var(poly_var&& rhs)
{
  std::memcpy(this, &rhs, sizeof(poly_var));
  poly_var_init(&rhs);
}

#endif //!defined(BOOST_NO_RVALUE_REFERENCES)

poly_var& poly_var::operator=(poly_var rhs) 
{ 
  swap(rhs); 

  return *this; 
}

void poly_var::swap(poly_var& rhs)
{
  poly_var_rep tmp;
  poly_var_init(&tmp);
  std::memcpy(&tmp, &rhs, sizeof(poly_var));
  std::memcpy(&rhs, this, sizeof(poly_var));
  std::memcpy(this, &tmp, sizeof(poly_var));
}

template<> int poly_var::as<int>() const 
{ 
  if(type != pv_type_int)
    throw bad_poly_var_cast();

  return val.w; 
}

template<> double poly_var::as<double>() const 
{ 
  if(type != pv_type_num)
    throw bad_poly_var_cast();

  return val.num; 
}

template<> char const* poly_var::as<char const*>() const 
{ 
  if(type != pv_type_str)
    throw bad_poly_var_cast();

  return val.str; 
}

template<> bool poly_var::as<bool>() const 
{ 
  if(type != pv_type_bool)
    throw bad_poly_var_cast();

  return val.bool_!=0; 
}

std::ostream& operator<<(std::ostream& os, poly_var_rep_t const& rhs)
{
  if(os.good())
  {
    switch(rhs.type)
    {
    case pv_type_nil:
    case pv_type_missing:
    {
      os << "<empty>";

      break;
    }
    case pv_type_int:
    {
      os << rhs.val.w;

      break;
    }
    case pv_type_num:
    {
      os << rhs.val.num;

      break;
    }
    case pv_type_bool:
    {
      os << std::boolalpha << bool(rhs.val.bool_!=0);

      break;
    }
    case pv_type_str:
    {
      os << rhs.val.str;

      break;
    }
    case pv_type_multi:
    {
      std::size_t m=rhs.val.array.rows, n=rhs.val.array.columns;
      poly_var_rep_t const* p=rhs.val.array.data;
      os << "[" << m << "x" << n << ":";
      while(p != rhs.val.array.data+m*n) os << ' ' << *p++;
      os << ']';

      break;
    }
    default:
    {
      assert(false);

      break;
    }
    }
  }

  return os;
}

/////////1////////2////////3////////4/////////5/////////6/////////7/////////8
// API implementation.

//PP-meta code for poly_var_api_call_by_name() (to follow).

#define POLY_VAR_TEXT(z, n, text) text

#define POLY_VAR_DISPATCH(n) \
  case n: \
  { \
    poly_var* (*f)(BOOST_PP_ENUM_PARAMS(n, poly_var const* arg)) = \
      reinterpret_cast<poly_var*(*)( \
         BOOST_PP_ENUM(n, POLY_VAR_TEXT, poly_var const*))>(i->function); \
   \
    return f(BOOST_PP_ENUM_PARAMS(n, arg)); \
  }\
  /**/

poly_var* poly_var_api_call_by_name(
  char const* tag
  ,BOOST_PP_ENUM_PARAMS(POLY_VAR_API_MAX_ARITY, poly_var const* arg))
{
#if defined(POLY_VAR_USE_BOOST_LAMBDA) //Prefer not to.
  namespace bll=boost::lambda ;
  std::vector<poly_var_function>::const_iterator i = 
    std::find_if(registered_functions.begin(), registered_functions.end()
  		 , bll::bind(&std::strcmp, tag, bll::bind(&poly_var_function::name, bll::_1)
        ) == 0
#else//Phoenix.

  std::vector<poly_var_function>::const_iterator i = 
    std::find_if(registered_functions.begin(), registered_functions.end()
     , boost::phoenix::bind(
          &std::strcmp, tag
        , boost::phoenix::bind(
            &poly_var_function::name
          , boost::phoenix::arg_names::arg1)
        ) == 0
   );

#endif//defined(POLY_VAR_USE_BOOST_LAMBDA)

  if(i == registered_functions.end())
  {
    return (poly_var*)0L;
  }

  switch(i->arity)
  {
  case 0:
    {
      poly_var* (*f)() = 
        reinterpret_cast<poly_var*(*)()>(i->function);

      return f();
    }
  case 1:
    {
      poly_var* (*f)(poly_var const* arg0) = 
        reinterpret_cast<poly_var*(*)(poly_var const*)>(i->function);

      return f(arg0);
    }

//Generate the remaining cases up to POLY_VAR_MAX_ARITY.

#define BOOST_PP_LOCAL_MACRO POLY_VAR_DISPATCH
#define BOOST_PP_LOCAL_LIMITS (2, POLY_VAR_API_MAX_ARITY)
#include BOOST_PP_LOCAL_ITERATE()    

  default:
    {

      return (poly_var*)0L;
    }
  }

  return (poly_var*)0L;
}

#undef POLY_VAR_DISPATCH
#undef POLY_VAR_TEXT

/////////1////////2////////3////////4/////////5/////////6/////////7/////////8
// Example API functions.

namespace zen
{

poly_var* pv_registered_functions()
{
  std::auto_ptr<poly_var> result(new poly_var(registered_functions.size(), 2));
  poly_var& lhs=*result;
  poly_var* data=result->data();
  BOOST_FOREACH(poly_var_function const& f, registered_functions)
  {
    poly_var name(f.name);
    poly_var arity(boost::numeric_cast<int>(f.arity));

    *data++ = name;
    *data++ = arity;
  }

  return result.release();
}

poly_var* pv_compiler()
{
  std::auto_ptr<poly_var> result(new poly_var(BOOST_COMPILER));

  return result.release();
}

poly_var* pv_sum(poly_var const* in)
{
  double v=0;
  BOOST_FOREACH(poly_var const& var
    , boost::make_iterator_range(in->data(), in->data()+in->rows()*in->columns()))
  {
    v += poly_var_is_int(&var) ? var.as<int>() : var.as<double>();
  }
  
  return new poly_var(v);
}

poly_var* pv_transpose(poly_var const* in)
{
  std::size_t m = in->rows(), n = in->columns();
  std::auto_ptr<poly_var> result(new poly_var(n, m));
  poly_var& lhs = *result;
  poly_var const& rhs = *in;
  for(std::size_t i = 0; i < m; ++i)
  {
    for(std::size_t j = 0; j < n; ++j)
    {
      lhs(j, i) = rhs(i, j);
    }
  }

  return result.release();
}

}//namespace zen

extern "C"
{
poly_var* pv_integers(poly_var const* from, poly_var const* to)
{
  int n = to->as<int>() - from->as<int>() + 1;
  std::auto_ptr<zen::poly_var> p(new zen::poly_var(n, 1));
  zen::poly_var& var=*p;
  for(int j=0, i = from->as<int>(); i <= to->as<int>(); ++i, ++j)
  {
    var(j, 0) = poly_var(i);
  }

  return p.release();
}

//This library's registration function.

#define POLY_VAR_FUNCTION_TABLE_BEGIN() \
  static poly_var_function entries[]= \
    { \
/**/

#define POLY_VAR_FUNCTION_ITEM(i, name, arity, func) \
  {i, #name, arity, reinterpret_cast<void(*)()>(&func)}, \
  /**/

#define POLY_VAR_FUNCTION_TABLE_END() \
  }; \
/**/

void poly_var_api_register_functions(std::vector<poly_var_function>& functions)
{
  POLY_VAR_FUNCTION_TABLE_BEGIN()
    POLY_VAR_FUNCTION_ITEM(0, compiler, 0, zen::pv_compiler)
    POLY_VAR_FUNCTION_ITEM(1, integers, 2, pv_integers)
    POLY_VAR_FUNCTION_ITEM(2, sum, 1, zen::pv_sum)
    POLY_VAR_FUNCTION_ITEM(3, transpose, 1, zen::pv_transpose)
    POLY_VAR_FUNCTION_ITEM(4, registered_functions, 0, zen::pv_registered_functions)
  POLY_VAR_FUNCTION_TABLE_END()

  poly_var_function const* first=entries;
  poly_var_function const* last=first+sizeof(entries)/sizeof(poly_var_function);

  BOOST_FOREACH(
     poly_var_function const& f, boost::make_iterator_range(first, last))
  {
    functions.push_back(f);
  }
}

}//extern "C"
