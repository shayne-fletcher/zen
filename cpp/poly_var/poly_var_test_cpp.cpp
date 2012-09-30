#include <cpp/poly_var/poly_var.h>

#include <boost/scoped_ptr.hpp>

#include <iostream>

typedef boost::scoped_ptr<poly_var> poly_var_ptr;

int main()
{
  poly_var_api_initialize();

  //What compiler?

  poly_var_ptr compiler(poly_var_api_call_by_name("compiler"));
  std::cout << compiler->as<char const*>() << std::endl;

  //Directly invoke integers().

  poly_var from(1), to(10);

  poly_var_ptr p(pv_integers(&from, &to));
  poly_var const& var1=*p;
  for(std::size_t i = 0; i < var1.rows(); ++i)
  {
    std::cout << var1(i, 0).as<int>() << " ";
  }
  std::cout << std::endl;

  //Invoke integers() indirectly by name.

  poly_var_ptr q(poly_var_api_call_by_name("integers", &from, &to));
  poly_var const& var2=*q;
  for(std::size_t i = 0; i < var2.rows(); ++i)
  {
    std::cout << var2(i, 0).as<int>() << " ";
  }
  std::cout << std::endl;

  poly_var_api_uninitialize();

  return 0;
}
