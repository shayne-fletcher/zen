'''Find a longest increasing subsequence
http://basicalgos.blogspot.com/2012/03/37-longest-increasing-sequence-lis.html

'''
import functools

def ics (s):  #s is s sequence
  """Find the set of increasing subsequences of s.

  """
  if len (s) == 1 :
      return [s] #s itself is an ics of length 1

  #This is the key point.
  ts = ics (s[1:]) #ts are the ics values of s - it's first element

  #Extend ts with any new subsequences that can be formed by
  #prepending s[0] to t for t in ts.
  def f (acc, t) :
    if s[0] < t[0]:
       nt = [s[0]]+t
       return acc + [nt]
    else:
        return acc
  res = functools.reduce (f, ts, ts) + [[s[0]]]

  return res

#s, the input sequence.

s=[0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]

#Print all longest increasing subsequences of s.

res = ics (s) #Find every ics of s
lens=[i for i in map (lambda x : len (x), res)] #Now their lengths
lismax= \
  functools.reduce (\
    lambda acc, y: max (acc, y), lens, 0) #The max length
for f in res:
    if len(f) == lismax: print (str(f)) #An ics of max length
