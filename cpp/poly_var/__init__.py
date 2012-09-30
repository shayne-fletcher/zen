import sys
from _poly_var import *

class __poly_var_function:
  """A callable object that redirects to a function in the poly_var API.

  """

  def __init__(self, details):
    """Squirrel away the function name and arity.

    """
    (self.__name, self.__arity) = (details[0], details[1])

  def __call__(self, *params):
    """Check the number of provided parameters matches the function arity and if it
    does, invoke it.

    """
    n, arity = (len(params), self.__arity)
    if(n != arity):
      raise TypeError(
        "\""+self.__name+"\" takes " +str(arity) +
        " arguments (" +str(n)+ " given)")
    
    return call_by_name(self.__name, *params)

def __generate_poly_var_api_functions():
  """Create a named __poly_var instance for each poly_var API function and 
     inject each such instance into the current module's namespace.

  """
  module=sys.modules[__name__]
  for f in call_by_name("registered_functions"):
    setattr(module, f[0], __poly_var_function(f))

__generate_poly_var_api_functions()
