import functools
_isconst = \
  lambda x :functools.reduce \
    (lambda acc, c : acc and isinstance (c, _const), x, True)

class _float :
  def __neg__ (self) :
    return _const (-self.f) if _isconst ([self]) else _neg (self)
  def __add__ (self, x) : 
      return _const (self.f + x.f) if _isconst ([self, x]) else \
      x if _isconst ([self]) and self.f == 0 else               \
      self if _isconst ([x]) and x.f == 0 else _add (self, x)
  def __sub__ (self, x) : 
      return _const (self.f - x.f) if _isconst ([self, x]) else \
      const (-x.f) if _isconst ([self]) and self.f == 0 else    \
      self if _isconst ([x]) and x.f == 0.0 else _sub (self, x)
  def __mul__ (self, x) : 
      return _const (self.f * x.f) if _isconst ([self, x]) else \
      x if _isconst ([self]) and self.f == 1 else               \
      self if _isconst ([x]) and x.f == 1 else _mul (self, x)
  def __div__ (self, x) : 
      return _const (self.f / x.f) if _isconst ([self, x]) else \
      self if _isconst([x]) and x.f == 1 else _div (self, x)

class _neg (_float):
  def __init__ (self, f) : self.f = f
  def __str__ (self) : return "-" + "(" + str(self.f) + ")"
class _fix (_float):
  def __init__ (self, d, f) : self.d = d; self.f = f
  def __str__ (self) : return "fix(" + str(self.d) +", " + str(self.f) + ")"
class _add (_float) :
  def __init__ (self, lhs, rhs) : self.lhs = lhs; self.rhs = rhs
  def __str__ (self) : return str(self.lhs)+ " + " + str(self.rhs)
class _sub (_float):
  def __init__ (self, lhs, rhs) : self.lhs = lhs; self.rhs = rhs
  def __str__ (self) : return str(self.lhs)+ " - " + str(self.rhs)
class _mul (_float):
  def __init__ (self, lhs, rhs) : self.lhs = lhs; self.rhs = rhs
  def __str__ (self) : return str (self.lhs)+ " * " + str (self.rhs)
class _div (_float):
  def __init__ (self, lhs, rhs) : self.lhs = lhs; self.rhs = rhs
  def __str__ (self) : return str (self.lhs)+ " / " + str (self.rhs)
class _const (_float):
  def __init__ (self, f) : self.f = f;
  def __str__ (self) : return str (self.f)
class _market (_float):
  def __init__ (self, tag) : self.tag = tag
  def __str__ (self) : return "market \"" + str(self.tag) + "\""
class _max (_float):
  def __init__ (self, lhs, rhs) : 
      self.lhs = lhs; self.rhs = rhs
  def __str__ (self) : 
    return "max(" + str (self.lhs) + ", " + str (self.rhs) + ")"
class _min (_float):
  def __init__ (self, lhs, rhs) : 
      self.lhs = lhs; self.rhs = rhs
  def __str__ (self): 
    return "min(" + str (self.lhs) + ", " + str (self.rhs) + ")"

def visit (f, acc, xpr):
  if isinstance (xpr, _const) : return f._const (acc, xpr)
  if isinstance (xpr, _neg) : return f._neg (acc, xpr)
  if isinstance (xpr, _fix) : return f._fix (acc, xpr)
  if isinstance (xpr, _market) : return f._market (acc, xpr)
  if isinstance (xpr, _add) : return f._add (acc, xpr)
  if isinstance (xpr, _sub) : return f._sub (acc, xpr)
  if isinstance (xpr, _mul) : return f._mul (acc, xpr)
  if isinstance (xpr, _div) : return f._div (acc, xpr)
  if isinstance (xpr, _max) : return f._max (acc, xpr)
  if isinstance (xpr, _min) : return f._min (acc, xpr)

  raise RuntimeError ("Expression match failure")

const = lambda c : _const (c)
market = lambda s : _market (s)
max_ = lambda a, b : _max (a, b)
min_ = lambda a, b : _min (a, b)

def fix (d, x):

  class __fix_visitor:
    def __init__ (self, d) : 
      self.d = d
    def _const (self, _, xpr) : 
      return xpr
    def _market (self, _, xpr) : 
      return _fix (self.d, xpr)
    def _fix (self, _, xpr) : return xpr
    def _neg (self, _, xpr) : 
      return _neg (visit (self, _, xpr.f))
    def _add (self, _, xpr) : 
      return _add (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _sub (self, _, xpr) : 
      return _sub (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _mul (self, _, xpr) : 
      return _mul (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _div (self, _, xpr) : 
      return _div (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _max (self, _, xpr) : 
      return _max (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _min (self, _, xpr) : 
      return _min (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))

    return visit (__fix_visitor (d), None, x)

def simplify (fs, x):

  class _apply_fixings_visitor :
    def __init__(self, fs) : self.fs = fs
    def _const (self, _, xpr) : return xpr
    def _market (self, _, xpr) : return xpr
    def _fix (self, _, xpr) : 
      fs = [f for f in self.fs if f[0] == xpr.f.tag and f[1] == xpr.d]
      return xpr if len (fs) == 0 else _const (fs[0][2])
    def _neg (self, _, xpr) : 
      return _neg (visit (self, _, xpr.f))
    def _add (self, _, xpr) : 
      return _add (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _sub (self, _, xpr) : 
      return _sub (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _mul (self, _, xpr) : 
      return _mul (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _div (self, _, xpr) : 
      return _div (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _max (self, _, xpr) : 
      return _max (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))
    def _min (self, _, xpr) : 
      return _min (visit (self, _, xpr.lhs), visit (self, _, xpr.rhs))

  class _simplify_visitor:
    def _const (self, _, xpr) : 
      return xpr
    def _fix (self, _, xpr) : 
      return xpr
    def _market (self, _, xpr) : 
      return xpr
    def _neg (self, _, xpr) : 
      f = visit (self, _, xpr.f)
      return xpr if not _isconst ([f]) else -f
    def _add (self, _, xpr) : 
      l = visit (self, _, xpr.lhs); r = visit (self, _, xpr.rhs)
      return xpr if not _isconst([l, r]) else const (l.f + r.f)
    def _sub (self, _, xpr) :
      l = visit (self, _, xpr.lhs); r = visit (self, _, xpr.rhs)
      return xpr if not _isconst([l, r]) else const (l.f - r.f)
    def _mul (self, _, xpr) :
      l = visit (self, _, xpr.lhs); r = visit (self, _, xpr.rhs)
      return xpr if not _isconst([l, r]) else const (l.f * r.f)
    def _div (self, _, xpr) :
      l = visit (self, _, xpr.lhs); r = visit (self, _, xpr.rhs)
      return xpr if not _isconst([l, r]) else const (l.f / r.f)
    def _max (self, _, xpr) :
      l = visit (self, _, xpr.lhs); r = visit (self, _, xpr.rhs)
      return xpr if not _isconst([l, r]) else const (max (l.f, r.f))
    def _min (self, _, xpr) :
      l = visit (self, _, xpr.lhs); r = visit (self, _, xpr.rhs)
      return xpr if not _isconst([l, r]) else const (min (l.f, r.f))

  return visit ( \
    _simplify_visitor (), None, visit (_apply_fixings_visitor (fs), None, x))
