#if !defined(POLY_VAR_69C0BD6A_7086_4119_B86C_A90E7076161F_H)
#  define POLY_VAR_69C0BD6A_7086_4119_B86C_A90E7076161F_H

#  if !defined(POLY_VAR_API_MAX_ARITY)
#    define POLY_VAR_API_MAX_ARITY 30
#  endif //!defined(POLY_VAR_API_MAX_ARITY)

#  include <cpp/poly_var/config.h>

#  include <stdint.h>
#  include <stdlib.h>

enum poly_var_rep_code
{
    pv_type_num     = 0x0001
  , pv_type_str     = 0x0002
  , pv_type_bool    = 0x0004
  , pv_type_multi   = 0x0040
  , pv_type_missing = 0x0080
  , pv_type_nil     = 0x0800
  , pv_type_int     = 0x0100
};

typedef struct POLY_VAR_DECL poly_var_rep
{
  union
  {
    int w;
    double num;
    char* str;
    uint16_t bool_;

    struct
    {
      struct poly_var_rep* data;
      size_t rows;
      size_t columns;
    } array;

  } val;
  uint16_t type;

} poly_var_rep_t;

#  if defined(__cplusplus)
extern "C"
{
#  endif /*defined(__cplusplus)*/

/*C api*/

POLY_VAR_DECL void poly_var_api_initialize();
POLY_VAR_DECL void poly_var_api_uninitialize();

POLY_VAR_DECL void poly_var_init(poly_var_rep_t* p);
POLY_VAR_DECL void poly_var_clear(poly_var_rep_t* p);
POLY_VAR_DECL void poly_var_copy(poly_var_rep_t* dst, poly_var_rep_t const* src);

POLY_VAR_DECL int poly_var_is_double(poly_var_rep_t const* p);
POLY_VAR_DECL int poly_var_is_string(poly_var_rep_t const* p);
POLY_VAR_DECL int poly_var_is_bool(poly_var_rep_t const* p);
POLY_VAR_DECL int poly_var_is_array(poly_var_rep_t const* p);
POLY_VAR_DECL int poly_var_is_missing(poly_var_rep_t const* p);
POLY_VAR_DECL int poly_var_is_nil(poly_var_rep_t const* p);
POLY_VAR_DECL int poly_var_is_int(poly_var_rep_t const* p);

POLY_VAR_DECL poly_var_rep_t* poly_var_api_call_by_name(char const* tag, size_t num_args, ...);

#  if defined(__cplusplus)
}//extern "C"
#  endif/*defined(__cplusplus)*/

#  if !defined(__cplusplus)

typedef poly_var_rep_t poly_var; /*C definition for poly_var.*/

#  else

//OMG C++! Much better :)

#  include <boost/config.hpp>
#  include <boost/preprocessor/repetition/enum_params_with_a_default.hpp>

#  include <cstdlib>
#  include <string>
#  include <iostream>
#  include <stdexcept>

struct POLY_VAR_DECL poly_var : poly_var_rep_t
{
  poly_var() { poly_var_init(this); }
  ~poly_var() { poly_var_clear(this); }
  explicit poly_var(int v);
  explicit poly_var(double v);
  explicit poly_var(char const* const s);
  explicit poly_var(std::string const& s);
  explicit poly_var(bool b);
  poly_var(std::size_t rows, std::size_t cols);
  poly_var(poly_var const& rhs) { poly_var_copy(this, &rhs); }
#  if !defined(BOOST_NO_RVALUE_REFERENCES)
  poly_var(poly_var&& rhs);
#  endif//!defined(BOOST_NO_RVALUE_REFERENCES)
  poly_var& operator=(poly_var rhs);
  poly_var& operator()(std::size_t i, std::size_t j){return *((poly_var*)val.array.data+i*val.array.columns+j);}
  poly_var const& operator()(std::size_t i, std::size_t j)const{return *((poly_var*)val.array.data+i*val.array.columns+j);}
  void swap(poly_var& rhs);
  std::size_t rows() const { return val.array.rows; }
  std::size_t columns() const { return val.array.columns; }
  poly_var* data() { return (poly_var*)val.array.data; }
  poly_var const* data() const { return (poly_var const*)val.array.data; }

  template<class T> T as() const;
};

struct bad_poly_var_cast : std::runtime_error
{
 bad_poly_var_cast() 
   : std::runtime_error("Bad poly_var cast")
    {}
};

template<> POLY_VAR_DECL int poly_var::as<int>() const;
template<> POLY_VAR_DECL double poly_var::as<double>() const;
template<> POLY_VAR_DECL char const* poly_var::as<char const*>() const;
template<> POLY_VAR_DECL bool poly_var::as<bool>() const;

POLY_VAR_DECL std::ostream& operator << (std::ostream& os, poly_var_rep_t const& rhs);

//C++ API

//The following generates the prototype
//  poly_var* poly_var_api_call_by_name(char const* name, poly_var const* arg0=0, ..., poly_var const* argN=0);
//where N is set to POLY_VAR_MAX_ARITY.

POLY_VAR_DECL poly_var* poly_var_api_call_by_name(char const* name, BOOST_PP_ENUM_PARAMS_WITH_A_DEFAULT(POLY_VAR_API_MAX_ARITY, poly_var const* arg, 0L));

namespace zen
{
  using ::poly_var;
  using ::poly_var_api_call_by_name;

}//namespace zen

#  endif //!defined(__cplusplus)

/*Example of an API function.*/

#  if defined(__cplusplus)
extern "C"
{
# endif /*defined(__cplusplus)*/

POLY_VAR_DECL poly_var* pv_integers(poly_var const* from, poly_var const* to);

#  if defined(__cplusplus)

}//extern "C"

# endif /*defined(__cplusplus)*/

#endif /*POLY_VAR_69C0BD6A_7086_4119_B86C_A90E7076161F_H*/
