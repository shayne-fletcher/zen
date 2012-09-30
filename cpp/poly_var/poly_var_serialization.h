#if !defined(POLY_VAR_SERIALIZATION_69DD8EEC_7BF3_4A64_92B6_6A50F7C72DFA_H)
#  define POLY_VAR_SERIALIZATION_69DD8EEC_7BF3_4A64_92B6_6A50F7C72DFA_H

#  include <cpp/poly_var/poly_var.h>

#  include <boost/serialization/split_free.hpp>
#  include <boost/serialization/nvp.hpp>
#  include <boost/serialization/array.hpp>

#  include <string>
#  include <cassert>
#  include <cstdlib>
#  include <cstring>

namespace boost {  namespace serialization {

template<class Archive>
inline void serialize(Archive& ar, poly_var_rep_t& var, unsigned int const version)
{
  split_free(ar, var, version);
}

template<class Archive>
inline void serialize(Archive& ar, poly_var& var, unsigned int const version)
{
  serialize(ar, (poly_var_rep_t&)var, version);
}

template<class Archive>
void save(Archive& ar, poly_var_rep_t const& var, unsigned int const /*version*/)
{
  ar & boost::serialization::make_nvp("type", var.type);

  switch(var.type)
  {
  case pv_type_int:
	{
	  ar & boost::serialization::make_nvp("value", var.val.w);
	  break;
	}
  case pv_type_num:
	{
	  ar & boost::serialization::make_nvp("value", var.val.num);
	  break;
	}
  case pv_type_str:
	{
	  std::string str(var.val.str); ar & boost::serialization::make_nvp("value", str);
	  break;
	}
  case pv_type_bool:
	{
	  ar & boost::serialization::make_nvp("value", var.val.bool_);
	  break;
	}
  case pv_type_multi:
	{
	  ar & boost::serialization::make_nvp("rows", var.val.array.rows);
	  ar & boost::serialization::make_nvp("columns", var.val.array.columns);
	  ar & boost::serialization::make_nvp("data", boost::serialization::make_array(var.val.array.data, var.val.array.rows*var.val.array.columns));
	}
  case pv_type_nil:
  case pv_type_missing:
  	  break;
  default:
  	  assert(false);
  }
}

template<class Archive>
inline void save(Archive& ar, zen::poly_var const& var, unsigned int const version)
{
  save(ar, (poly_var_rep_t const&)var, version);
}

template<class Archive>
void load(Archive& ar, poly_var_rep_t& var, unsigned int const)
{
  ar & boost::serialization::make_nvp("type", var.type);

  switch(var.type)
  {
  case pv_type_int:
	{
	  ar & boost::serialization::make_nvp("value", var.val.w);

	  break;
	}
  case pv_type_num:
	{
	  ar & boost::serialization::make_nvp("value", var.val.num);

	  break;
	}
  case pv_type_str:
	{
	  std::string s;
	  ar & boost::serialization::make_nvp("value", s);
	  char* str = (char*)std::malloc((s.size()+1)*sizeof(char));
	  std::strcpy(str, s.c_str());
	  str[s.size()] = '\0';
	  var.val.str = str;

	  break;
	}
  case pv_type_bool:
	{
	  ar & boost::serialization::make_nvp("value", var.val.bool_);

	  break;
	}
  case pv_type_multi:
	{
	  ar & boost::serialization::make_nvp("rows", var.val.array.rows);
	  ar & boost::serialization::make_nvp("columns", var.val.array.columns);
	  std::size_t n = var.val.array.rows*var.val.array.columns;
	  var.val.array.data = (poly_var_rep_t*)std::malloc(n*sizeof(poly_var_rep_t));
	  std::memset(var.val.array.data, 0, n*sizeof(poly_var_rep_t));
	  ar & boost::serialization::make_nvp("data", boost::serialization::make_array(var.val.array.data, n));
	}
  case pv_type_nil:
  case pv_type_missing:
	{
	  break;
	}
  default:
	{
	  assert(false);
	}
  }
}

template<class Archive>
inline void load(Archive& ar, zen::poly_var& var, unsigned int const version)
{
  load(ar, (poly_var_rep&)var, version);
}

}}//namespace boost::serialization

#endif//!defined(POLY_VAR_SERIALIZATION_69DD8EEC_7BF3_4A64_92B6_6A50F7C72DFA_H)
