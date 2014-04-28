#include <boost/math/distributions/normal.hpp>

#include <caml/mlvalues.h>
#include <caml/alloc.h>

extern "C" value caml_norm_cdf (value v)
{
  using boost::math::cdf;
  using boost::math::normal;

  return caml_copy_double (cdf (normal (0.0, 1.0), Double_val (v)));
}
