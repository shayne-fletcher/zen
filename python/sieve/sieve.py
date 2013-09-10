#sieve.py
#
#Find all the prime numbers less than or equal to k using the sieve of
#Eratosthenes.
#
#http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

import functools

k=1000

def sieve (k):

  def f (p, lst):
    s = list (filter (lambda i : i%p != 0 or i/p == 1, lst))
    r = list (filter (lambda i : i > p, s))
    if len (r) == 0:
        return s
    else:
        return f (r[0], s)

  return f (2, range (2, k + 1))

print (str(sieve (k)))
