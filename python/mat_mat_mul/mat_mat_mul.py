import functools

def proj_r (A, i):
  return A[i]

def proj_c (A, j):
  return tuple (
      map (lambda t: (t[j],), A))

def scale_r (u, lam): 
  return tuple (map (lambda ui: lam*ui, u))

def scale_c (c, k):
  def f (e):
    (xi,) = e
    return k*xi,
  return tuple (map (f, c))

# >>>c = ((1,),(2,))
# >>>print (str (scale_c (c, 2.0)))
# ((2.0,), (4.0,))

def transpose (A):
  M = tuple (map (
          lambda x : tuple (
              map (lambda e: (e,), x)), A))
  def f (j):
    def nth (i):
      return lambda t : t[i]
    return functools.reduce (
      lambda x, y : x + y, 
                  tuple (map (nth (j), M)), ())
  return tuple (map (f, range (0, len (A[0]))))

#flatten reinterprets a sequence of column vectors as a matrix of row
# vectors
#
# e.g.
#   >>> t1 = (1,), (2,) 
#   >>> t2 = (3,), (4,) 
#   >>> A = (t1, t2)
#   >>> print (str (flatten (A)))
#   ((1, 3), (2, 4))

def flatten (t):
  def row(i):
    def f (acc, y): 
        return acc + y[i]
    return functools.reduce (f, t, ())
  return tuple (map (row, range (0, len (t[0]))))

def mat_vec_mul (A, x):
  def f (i):
      (xi,) = x[i]
      return scale_c (proj_c (A, i), xi)
  M = tuple (map (f, range (0, len (x))))
  return tuple (map (lambda r : (sum (r), ), flatten (M)))

# >>> A = ((2, 3), (4, 0) )A = ((2, 3), (4, 0) )
# >>> c = ((1,), (2,))
# >>> print (str (mat_vec_mul (A, c)))
# ((8,), (4,))

def mat_mat_mul (A, B):
  f = lambda j : mat_vec_mul (A, proj_c (B, j))
  return flatten (tuple (map (f, range (0, len (B[0])))))

A = ((2, 3), (4, 0) )
B = ((1,  2, 0), (5, -1, 0) )
print (str (mat_mat_mul (A, B)))
