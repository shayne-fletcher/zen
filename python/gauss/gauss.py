import functools

def scale_row (u, lam): 
    return tuple (
      map (lambda ui: lam*ui, u))

def subtract_rows (u, v) :
  return tuple (
    map (lambda i : u[i] - v[i], range (0, len (u))))

def elimination_phase (t):
  def elimination_step (tp, k): 
    pr = tp[k]
    def g (e):
      (i, r) = e
      if i <= k : 
        return r
      else:
        lam = r[k]/pr[k]
        return subtract_rows (r, scale_row (pr, lam))
    return tuple (
             map (g, tuple (
                      zip (range (0, len (tp)), tp))))
  return functools.reduce (
              elimination_step, range (0, len (t)), t)

def dot (u, v):
  def f (acc, i):
    (a, (b,)) = (u[i], v[i])
    return acc + a*b
  return functools.reduce (f, range (0, len (u)), 0.0)

def back_substitution_phase (t):
  n = len (t)
  def back_substitution_step (x, k):
    bk = (t[k])[n]
    akk = (t[k])[k]
    xk = bk/akk if k == n-1 \
      else (bk - dot ((t[k][k+1:])[0:n-k-1], x))/akk
    return ((xk,),) + x
  return functools.reduce (
    back_substitution_step, range (len (t)-1, -1, -1), ())

#Solve Ax=b with 
#
#  A=[ 6   -4     1
#     -4    6    -1
#      1   -4     6]
#
#and b = (-14, 36, 6)^T.

A = (
    ( 6.,  -4.,  1.,  -14.)
  , (-4.,   6., -4.,   36.)
  , ( 1.,  -4.,  6.,    6.))
print (str(back_substitution_phase (elimination_phase (A))))

#Should give (10, 22, 14)^T.
