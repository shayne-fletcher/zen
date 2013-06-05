=====================================================
Functional programming : More adventures in recursion
=====================================================

:Author: Shayne Fletcher

.. image:: /img/shayne_blue.jpg
   :align: left

:Date: Mar 2012
:Copyright: Copyright |copy| 2012, Bloomberg L.P. |---| All rights
            reserved.
:Abstract: A program to compute powersets.

.. |copy| unicode:: 0xA9 .. copyright sign
.. |---| unicode:: U+02014 .. em dash
   :trim:

.. .. contents::

----

Introduction
============

In this adventure, I was concerned with finding an algorithm to
compute the powerset of a given set. For example, given the set ``[1,
2, 3]`` the program should compute ``[[1, 2, 3], [1, 2], [1, 3], [2,
3], [1], [2], [3], []]``.

Show me the code
================

Here's a solution written in `Felix <http://www.felix-lang.org/>`_.

::

  fun find_seq[T] (k:size) (l:list[T]) : list[list[T]] =
  {
    return
      if k > len l
      then
        list[list[T]] () //There are no subsets of length k.
      elif k == 0uz
      then
        list[list[T]] (list[T] ()) //The empty set (the one subset of length zero).
      else
        //Those k length subsets that contain the head of the list
        //concatenated with those k length subsets that don't :)
        match l with
          | Cons(?x, ?xs) => 
             join 
              (map (fun (l:list[T]):list[T]=>
                           (join (list[T] x) l)) (find_seq (k - 1) xs))
              (find_seq k xs)
        endmatch
      endif
    ;
  }
  
  fun power_set[T] (lst:list[T]):list[list[T]] =
  {
    fun loop[T] (k:size) (lst:list[T]) (acc:list[list[T]]):list[list[T]] =
    {
      return
        if k == size(-1) then acc
        else loop (k - 1) lst (join acc (find_seq k lst))
        endif
      ;  
    }
  
   return loop (len lst) lst (list[list[T]] ());
  }
  
  println$ str (power_set (list (1, 2, 3)));
  
