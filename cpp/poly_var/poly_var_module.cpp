#include <cpp/poly_var/poly_var.h>

#include <boost/shared_ptr.hpp>
#include <boost/preprocessor/arithmetic/add.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>

#define BOOST_PYTHON_MAX_ARITY 32 //Change this as neccessary given
				  //the constraint below.
#include <boost/static_assert.hpp>
//Boost.Python needs to support a max arity of at least one more than
//the poly_var API.
BOOST_STATIC_ASSERT(
  BOOST_PYTHON_MAX_ARITY >= BOOST_PP_ADD(POLY_VAR_API_MAX_ARITY, 1)
);
#include <boost/python.hpp>

#include <iostream>
#include <iomanip>
#include <new>

namespace
{

typedef boost::shared_ptr<poly_var const> poly_var_ptr;

/////////1////////2////////3////////4/////////5/////////6/////////7/////////8
// C++ to Python

struct poly_var_to_python_converter
{
  static PyObject* convert(poly_var const& var)
  {
    if(poly_var_is_missing(&var)
       || poly_var_is_nil(&var))
    {
      return Py_None;
    }

    if(poly_var_is_string(&var))
    {
      boost::python::return_by_value::apply<std::string>::type converter;
      
      return converter(std::string(var.as<char const*>()));
    }

    if(poly_var_is_bool(&var))
    {
      boost::python::return_by_value::apply<bool>::type converter;

      return converter(var.as<bool>());
    }

    if(poly_var_is_double(&var))
    {
      boost::python::return_by_value::apply<double>::type converter;
      
      return converter(var.as<double>());
    }

    if(poly_var_is_int(&var))
    {
      boost::python::return_by_value::apply<int>::type converter;
      
      return converter(var.as<int>());
    }

    if(poly_var_is_array(&var))
    {
      // if(var.columns() == 1)
      // {
      // 	boost::python::list row;
      // 	for(std::size_t i = 0; i < var.rows(); ++i)
      // 	{
      // 	  row.append<poly_var>(var(i, 0));
      // 	}
      // 	boost::python::return_by_value::apply<boost::python::list>::type converter;
      // 	return converter(row);
      // }

      boost::python::list matrix;
      for(std::size_t i = 0; i < var.rows(); ++i)
      {
	boost::python::list row;

	for(std::size_t j = 0; j < var.columns(); ++j)
        {
	  row.append<poly_var>(var(i, j));
	}

	matrix.append<boost::python::list>(row);
      }

      boost::python::return_by_value::apply<boost::python::list>::type converter;

      return converter(matrix);
    }

    std::cout << "Unhandled case" << std::endl;

    //Never get here.

    return (PyObject*)0L;
  }

  static PyObject* convert(poly_var_ptr const& var)
  {
    return convert(*var);
  }

};

/////////1////////2////////3////////4/////////5/////////6/////////7/////////8
// Python to C++

struct is_none
{
  bool operator()(PyObject const* p) const 
  {
    return p == Py_None; 
  }
};
  
struct is_bool
{
  bool operator()(PyObject const* p) const 
  {
    return PyBool_Check(p); 
  }
};

struct is_int
{
  bool operator()(PyObject const* p) const 
  { 
#if PY_MAJOR_VERSION < 3
    //Python 2.x
    return !PyBool_Check(p) &&  (PyInt_Check(p) || PyLong_Check(p));
#else
    //Python 3.x
    return !PyBool_Check(p) &&  PyLong_Check(p);
#endif//PY_VERSION_HEX < 0x02060000
  }
};
  
struct is_double
{
  bool operator()(PyObject const* p) const 
  { 
    return PyFloat_Check(p);
  }
};

// From http://python3porting.com/cextensions.html:
//
//   Another change in the object header is that the PyObject_HEAD macro
//   has changed so that ob_type is now in a nested structure. This means
//   you no longer can pick the ob_type directly from the struct, so code
//   like ob->ob_type stops working. You should replace this with
//   Py_TYPE(ob). The Py_TYPE macro doesn’t appear until Python 2.6, so to
//   support earlier versions we make another #ifndef:
//
//   #ifndef Py_TYPE
//       #define Py_TYPE(ob) (((PyObject*)(ob))->ob_type)
//   #endif
//
//The Boost.Python header detail/wrap_python.hpp takes care of this for
//us.

struct is_sequence
{
  bool operator()(PyObject const* obj) const
  {
    return (PyList_Check(obj)
             || PyTuple_Check(obj)
             || PyIter_Check(obj)
             || PyRange_Check(obj)
             || (!PyBytes_Check(obj)
                    && !PyUnicode_Check(obj)
   		    && (Py_TYPE(obj) == 0
			|| Py_TYPE(Py_TYPE(obj)) == 0
                        || Py_TYPE(Py_TYPE(obj))->tp_name == 0
                        || std::strcmp(Py_TYPE(Py_TYPE(obj))->tp_name, "Boost.Python.class") != 0
		    )
                    && PyObject_HasAttrString(const_cast<PyObject*>(obj), "__len__")
                    && PyObject_HasAttrString(const_cast<PyObject*>(obj), "__getitem__"))
	    );
  }
};

template<class T, class PredT>
struct from_python_converter
{
  from_python_converter()
  {
    boost::python::converter::registry::push_back(
      &convertible, &construct, boost::python::type_id<poly_var>()
    );
  }

  static void* convertible(PyObject* obj)
  {
    PredT pred;

    return pred(obj) ? obj : 0L;
  }

  static void construct(
    PyObject* obj
    , boost::python::converter::rvalue_from_python_stage1_data* data)
  {
    typedef boost::python::converter::rvalue_from_python_storage<poly_var> storage_t;
    void* storage = reinterpret_cast<storage_t*>(data)->storage.bytes;
    new (storage) poly_var(boost::python::extract<T>(obj));
    data->convertible = storage;
  }
};

template<>
struct from_python_converter<void, is_none>
{
  from_python_converter()
  {
    boost::python::converter::registry::push_back(
      &convertible, &construct, boost::python::type_id<poly_var>());
  }

  static void* convertible(PyObject* obj)
  {
    return is_none()(obj) ? obj : 0L;
  }

  static void construct(
    PyObject* obj
    , boost::python::converter::rvalue_from_python_stage1_data* data)
  {
    typedef boost::python::converter::rvalue_from_python_storage<poly_var> storage_t;
    void* storage = reinterpret_cast<storage_t*>(data)->storage.bytes;
    new (storage) poly_var();
    data->convertible = storage;
  }

};

template <>
struct from_python_converter<poly_var, is_sequence>
{
  from_python_converter()
  {
    boost::python::converter::registry::push_back(
      &convertible, &construct, boost::python::type_id<poly_var>());
  }
  
  static void* convertible(PyObject* obj)
  {
  return is_sequence()(obj) ? obj : 0L;
  }
  
  static void construct(
    PyObject* obj_ptr
    , boost::python::converter::rvalue_from_python_stage1_data* data)
  {
    typedef boost::python::converter::rvalue_from_python_storage<poly_var> storage_t;
    void* storage = reinterpret_cast<storage_t*>(data)->storage.bytes;         
    boost::python::api::object obj(boost::python::handle<>(boost::python::borrowed(obj_ptr)));
            
    int x_len = boost::python::len(obj);      
  
    if(x_len == 0)
    {
      new (storage) poly_var();
      data->convertible = storage;        
    }
    else if(is_sequence()(boost::python::api::object(obj[0]).ptr()))
    {                           
      int y_len = boost::python::len(obj[0]);
      new (storage) poly_var(x_len, y_len);
      data->convertible = storage;
      poly_var& rng = *((poly_var*)storage);
          
      for(int i = 0; i < x_len; ++i)
        for(int j = 0; j < y_len; ++j)
        {
          if (len(obj[i]) != y_len)
            throw std::runtime_error("Jagged list detected");

          rng(i, j) = boost::python::extract<poly_var>(obj[i][j])();
        }
    }
    else
    {
      new (storage) poly_var(1, x_len);
      data->convertible = storage;
      poly_var& rng = *((poly_var*)storage);
      for (int i = 0; i < x_len; ++i)
      {
        rng(0, i) = boost::python::extract<poly_var>(obj[i])();
      }
    }
  }
};

void register_poly_var_python_converters()
{
  using boost::python::to_python_converter;
  using boost::python::implicitly_convertible;

  //C++ to Python (poly_var -> PyObject*)
  
  to_python_converter<poly_var, poly_var_to_python_converter>();
  to_python_converter<poly_var_ptr, poly_var_to_python_converter>();
  
  //Python to C++
  
  //Alas, implicitly_convertible *can't* be used with numbers when we
  //need booleans to work unless we recompile boost::python with a
  //special (undocumented) flag.  I've built a custom converter for
  //numbers instead (from_python_converter<>)
  //http://boost.2283326.n4.nabble.com/Avoiding-implicit-boost-python-extract-lt-gt-conversions-td3433520.html
  //
  //boost::python::implicitly_convertible<bool, poly_var>(); 
  //boost::python::implicitly_convertible<int, poly_var>();
  //boost::python::implicitly_convertible<double, poly_var>();
         
  implicitly_convertible<std::string, poly_var>(); // Strings are ok though.

  from_python_converter<void, is_none>(); //Python None -> poly_var
  from_python_converter<int, is_int>();   //Python int -> poly_var
  from_python_converter<bool, is_bool>();  //Python bool -> poly_var
  from_python_converter<double, is_double>(); //Python double ->poly_var
  from_python_converter<poly_var, is_sequence>(); //Python lists, tuples -> poly_var

}
  
poly_var_ptr call_by_name(
  char const* name
  , BOOST_PP_ENUM_PARAMS_WITH_A_DEFAULT(POLY_VAR_API_MAX_ARITY, poly_var const& arg, poly_var()))
{
  return poly_var_ptr(poly_var_api_call_by_name(name, BOOST_PP_ENUM_PARAMS(POLY_VAR_API_MAX_ARITY, &arg)));
}

BOOST_PYTHON_FUNCTION_OVERLOADS(call_by_name_overloads, call_by_name, 1, BOOST_PP_ADD(POLY_VAR_API_MAX_ARITY, 1))

}//namespace<anonymous>

BOOST_PYTHON_MODULE(_poly_var)
{
  using namespace boost::python;

  poly_var_api_initialize();

  register_poly_var_python_converters();

  def("call_by_name", call_by_name, call_by_name_overloads());
}
