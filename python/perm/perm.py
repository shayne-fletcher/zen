"""perm.py

Find all the permutations of a list. For example, given the list ``[1,
2, 3]`` compute all of ``[[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1],
[3, 1, 2], [3, 2, 1]]``.

The reasoning here goes like this. Suppose I know how to compute all
the permutations of a list that start with the element at position
*k*. Then the list of all permutations can be obtained by calling that
function with *k = 0, ..., N-1* where *N* is the length of the
list. How do you compute the permutations of a list that start with
the element at position *k*? Well, you compute all the permutations of
the reduced list that doesn't contain the *kth* element and prepend
them all with the *kth* element.

"""

def take(k, lst):
   '''The list slice, lst[0:k].
  
      >>> print (str(take(0, [1, 2, 3])))
      []
      >>> print (str(take(1, [1, 2, 3])))
      [1]
      >>> print (str(take(2, [1, 2, 3])))
      [1, 2]
      >>> print (str(take(3, [1, 2, 3])))
      [1, 2, 3]
      >>> print (str(take(4, [1, 2, 3])))
      [1, 2, 3]
  
   '''
   if(k <= 0):
       return []
   if(len(lst) == 0):
       return []
   res = [lst[0]]+take(k-1, lst[1:])

   return res
  
def drop(k, lst):
    '''The list slice lst[k+1:].
  
       >>> print (str(drop(0, [1, 2, 3])))
       [1, 2, 3]
       >>> print (str(drop(1, [1, 2, 3])))
       [2, 3]
       >>> print (str(drop(2, [1, 2, 3])))
       [3]
       >>> print (str(drop(3, [1, 2, 3])))
       []
       >>> print (str(drop(4, [1, 2, 3])))
       []
  
    '''
    if(k <= 0):
        return lst
    if(len(lst)==0):
        return []
    return drop(k-1, lst[1:])
  
def find_all_permutations(lst):
  '''Find all permutations of a list.
  
     >>> print (str(find_all_permutations([1, 2, 3])))
     [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
  
  '''
  def find_k_permutations(k, lst):
    '''Find all permutations of the given list that start with the
       element at index k.
  
    '''
    x = lst[k]
    listp=take(k, lst)+drop(k+1, lst)
    ps=find_all_permutations(listp)
    s = list (map (lambda perm : [x] + perm, ps))

    return s
  
  def loop(k, acc, lst):
      if(k == len(lst)):
          return acc
      else:
          return loop(k+1, acc + find_k_permutations(k, lst), lst)
  
  return ([[]], loop(0, [], lst))[len(lst)!=0]
  
if __name__ == "__main__":
  
    import doctest
    doctest.testmod() #No output means the tests pass or run from the
                      #command line with -v for verbose output.
  
