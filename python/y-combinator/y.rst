The Y combinator is a higher-order funtion. It takes a single argument
which is a function that isn't recursive and returns a version of the
function that is.

The goal here is to derive this function 'Y'.

Start with the classic recursive definition of the factorial function.
::

  def fact (n) :
    if n == 0 : 
      return 1 
    else: 
      return n * fact (n - 1)

We are trying to eliminate explicit recursion. To that end, factor out
the recursive call and make it an application of some continuation.
::

  def part_fact (this, n):
    if n == 0 :
      return 1
     else:
       return n * this (this, (n - 1))

  fact = functools.partial(part_fact, part_fact)

Next let's get this down to a function in one argument as is this way
in the lambda calculus.
::

  def part_fact (this):
    def aux (n):
      if n == 0 :
        return 1
      else:
        return n * (this (this)) (n - 1))
    return aux

  fact = part_fact (part_fact)

We'd recover something tantalizingly close to the original factorial
function if we factored out 'f (f)' into a function of one
argument.
::

  def part_fact (this):
    f = this (this)
    return lambda n : 1 if n == 0 else n * f (n - 1)

This would be fine in a lazy language but Python is strict and
exhibits infinite recursion because to evaluate 'part_fact
(part_fact)' requires evaluating 'f = part_fact (part_fact)' and so
on. The solution is to delay the evaluation until it's needed.
::

  def part_fact (this):
    f = lambda y : (this (this)) y
    return lambda n : 1 if n == 0 else n * f (n - 1)
  fact = part_fact(part_fact)

Refactor this into two parts.
::

  almost_fact = \
    lambda f : lambda n : 1 if n == 0 else f (n - 1)

  def part_fact (this):
    f = lambda y : (this (this))(y)
    return almost_fact (f)

Rephrase 'part_frac' as a lambda and change the argument name to 'x'.
::

  almost_fact = lambda f : lambda n : 1 if n == 0 else f (n - 1)
  part_fract = lambda x : amlost_fact (lambda y : (x (x))(y))
  fact = part_fact (part_fact)

Eliminate 'part_fact'.
::

  almost_fact = lambda f : lambda n : 1 if n == 0 else f (n - 1)
  fact = (lambda x : almost_fact (lambda y : (x (x))(y))) (lambda x : almost_fact (lambda y : (x (x))(y)))

That's it, there's the Y-combinator. Generalize!
::

  def Y (f):
   return (lambda x : f (lambda y : (x (x))(y))) (lambda x : f (lambda y : (x (x))(y)))

  almost_fact = lambda f : lambda n : 1 if n == 0 else n * f (n - 1)
  fact = Y (almost_fact)

Try this on another function. Fibonacci numbers say.
::

  def almost_fib (f) :
    return lambda n : 1 if n <= 2 else f (n - 1) + f (n - 2)
  fib = Y (almost_fib)

  print (str (fib (6))+"\n") #Prints '8'
