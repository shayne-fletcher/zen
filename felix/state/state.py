class state: #State s a
  def __init__(self, run):
      self.run = run # s -> (a*s)
  
  #(>>=) :: State s a -> (a -> State s b) -> State s b
  def bind(self, f):
      run0 = self.run
      def run1(state0):
          (result, state1) = run0(state0)
          return f(result).run(state1)
      return state(run1)
  def __rshift__(self, f): return self.bind(f)
  
  @classmethod
  def return_(cls, a) : # a ->State s a 
   return cls(lambda s : (a, s))
  
#Test 
  
class store:
  def __init__(self,a, b):
      self.a = a #running total,
      self.b = b#number of modifications (say)
  def __repr__(self):
    return "store(%d, %d)" % (self.a, self.b)
  
s0 = store(23, 4)
  
increment_store = state(lambda s : (s.a, store(s.a+1, s.b+1)))
increment_store_twice = increment_store >> (lambda _ : increment_store)
  
print ( str ( increment_store_twice.run(s0) ) )
  
