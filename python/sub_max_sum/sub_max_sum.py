'''Find sub-arrays of maximal sum

   You are given an array of n integers, each of which may be
   positive, negative or zero. Give an algorithm to identify the start
   and end index, i and j, of the interval whose elements form the
   maximal sum of all possible intervals. Assume j >=i

   e.g. {1 3 -8 2 -1 10 -2 1} -> i=3, j=5 - sum=11

   http://basicalgos.blogspot.com/2012/05/52-find-sub-array-of-max-sum.html
'''

import functools

#Compute all the intervals of the sequence t per the definition above.
#e.g. for [1, 2, e] compute {[1], [1, 2], [1, 2, 3], [2], [2, 3],
#[3]}. Decorate the intervals with their start and end indices and the
#interval sum.
def intervals (t):
  def f (acc, i):
    def row (i, t):
      s = t[i:]
      def f (acc, j):
         indices=(i, i+j-1)
         interval=s[0:j]
         return acc + [(indices, interval, sum (interval, 0))]
      return functools.reduce (f, list (range (1, len (s) + 1)), [])
    return acc + row (i, t)
  return functools.reduce (f, list (range (0, len (t))), [])
  
#x, the input sequence.
x=[1, 3, -8, 2, -1, 10, -2, 1]

#Print the intervals of x with maximal sum.
res = intervals (x)
def f (m, t): 
  (_,_,n)=t ; return max (m, n)
maxsum = functools.reduce (f, res, 0)
for t in res:
  (indices, interval, total)=t
  if (total == maxsum):
    print (str(t)) #An interval of maximal sum.
