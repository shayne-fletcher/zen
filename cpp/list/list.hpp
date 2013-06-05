#if !defined(LIST_1CD56401_BB89_4D12_9D83_1A17B0A647C3_INCLUDED)
# define LIST_1CD56401_BB89_4D12_9D83_1A17B0A647C3_INCLUDED

# if defined(_MSC_VER) && (_MSC_VER >= 1020)
#   pragma once
# endif// defined(_MSC_VER) && (_MSC_VER >= 1020)

#  include <boost/iterator/filter_iterator.hpp>
#  include <boost/iterator/zip_iterator.hpp>
#  include <boost/iterator/counting_iterator.hpp>
#  include <boost/spirit/home/phoenix/core.hpp>
#  include <boost/spirit/home/phoenix/operator.hpp>
#  include <boost/range.hpp>
#  include <boost/foreach.hpp>

#  include <iostream>
#  include <vector>
#  include <algorithm>
#  include <iterator>
#  include <stdexcept>
#  include <cctype>
#  include <string>
#  include <functional>
#  include <string>

//! \mainpage "List" index
//!
//! \section intro_sec Introduction
//!
//! A library of list manipulation functions in C++ inspired by the Haskell
//! prelude.
//!
//! \author Shayne Fletcher
//! \date August, 2009
//! \version 0.0
//!
//! \section install_sec Installation
//!
//! This is a header only library. Simply include "list.hpp" to use the
//! functions in this library.

//! \namespace list Selected Haskell prelude list manipulation functions.

//! Although this library implements for the most part a fair
//! selection of Haskell standard prelude functions it does also offer
//! some other 'interesting' algorithms, for example, "merge sort" and
//! "cartesian product".

namespace list
{

  //! Push the contents of a range onto an output stream.

  template <class SrcT>
  void show(SrcT const& src, std::ostream& os)
  {
    //! \tparam SrcT The source range type.
    //! \param src The range to 'show';
    //! \param os  The output stream to 'show' to.

    if(os.good())
    {
      std::copy(
          boost::begin(src)
        ,   boost::end(src)
        , std::ostream_iterator<
            typename boost::range_value<SrcT>::type>(os, "\n"));
      os << std::endl;
    }
  }

  //! Compute the length of a range.

  //! \verbatim
  //! len :: Num a => [b] -> a
  //! len [] = 0
  //! len (x:xs) = 1 + len xs
  //! \endverbatim

  template <class SrcT>
  inline std::size_t length(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range.
    //! \return Source range length.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::next;
    using boost::make_iterator_range;

    return !empty(src) ?
      1 + list::length(
            make_iterator_range(next(begin(src)), end(src))) : 0;
  }

  //! Reverse the order of the elements of a range.

  //! \verbatim
  //! reverse :: [a] -> [a]
  //! reverse [] = []
  //! reverse (x:xs) = reverse xs ++ [x]
  //! \endverbatim

  template <class SrcT, class DstT>
  inline void reverse(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::next;
    using boost::make_iterator_range;

    if(!empty(src))
    {
      list::reverse(
        make_iterator_range(next(begin(src)), end(src)), dst);
      *dst++ = *begin(src);
    }
  }

  //! Filter elements from a range according to a predicate.
  
  template <class SrcT, class DstT, class PredT>
  void filter(SrcT const& src, DstT dst, PredT pred)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type;
    //! \tparam PredT Predicate type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \param pred Predicate.

    using boost::begin;
    using boost::end;
    using boost::make_filter_iterator;

    std::copy(
        make_filter_iterator(pred, begin(src), end(src))
      , make_filter_iterator(pred, end(src), end(src))
      , dst);
  }
  
  //! Apply a function to each of the elements of a range.

  template <class SrcT, class DstT, class F>
  inline void map(SrcT const& src, DstT dst, F f)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type;
    //! \tparam F Function (object) type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \param f Function to apply.

    std::transform(boost::begin(src), boost::end(src), dst, f);
  }

  //! List comprehension. Equivalent to a filter followed by a map.

  template <class SrcT, class DstT, class PredT, class F>
  void comprehend(SrcT const& src, DstT dst, F f, PredT pred)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type;
    //! \tparam PredT Predicate type;
    //! \tparam F Function (object) type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \param pred Predicate to filter upon;
    //! \param f Function to apply.

    using boost::begin;
    using boost::end;
    using boost::make_iterator_range;
    using boost::make_filter_iterator;

    list::map(
        make_iterator_range(
            make_filter_iterator(pred, begin(src), end(src))
          ,   make_filter_iterator(pred, end(src), end(src))
          )
      , dst
      , f);
  }

  //! Sort the elements of a range using the "quick sort" algorithm.

  //! \verbatim
  //! quick_sort :: [a] -> [a]
  //! quick_sort [] = []
  //! quick_sort (x:xs) =
  //!  quick_sort [y | y <- xs, y <= x ] ++ [x] ++ quick_sort [y | y <- xs, y > x]
  //! \endverbatim

  template <class SrcT, class DstT>
  void quick_sort(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::phoenix::arg_names::arg1;
    using boost::make_iterator_range;
    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;

    typedef typename boost::range_value<SrcT>::type value_type;

    if(!empty(src))
    {
      value_type x=*boost::begin(src);

      std::vector<value_type> left, sleft;
      list::filter(
        make_iterator_range(
            next(begin(src))
          , end(src))
        , std::back_inserter(left)
        , arg1 <= x);
      list::quick_sort(left, std::back_inserter(sleft));
      std::copy(sleft.begin(), sleft.end(), dst);

      *dst++ = x;

      std::vector<value_type> right, sright;
      list::filter(
        make_iterator_range(
            next(begin(src))
          , end(src))
        , std::back_inserter(left)
        , arg1 > x);
      list::quick_sort(right, std::back_inserter(sright));
      std::copy(sright.begin(), sright.end(), dst);
    }
  }

  //! Take the first N elements of a range.

  //! \verbatim
  //! tak :: Num a => [b] -> a -> [b]
  //! tak [] _ = []
  //! tak _ 0 = []
  //! tak (x:xs) i = x : tak xs (i-1)
  //! \endverbatim

  template <class SrcT, class DstT>
  void take(SrcT const& src, DstT dst, std::size_t how_many)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \param how_many The number of elements to take.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    typedef typename boost::range_value<SrcT>::type value_type;

    if(how_many)
      if(!empty(src))
      {
        value_type x = *begin(src);
        *dst++ = x;
        list::take(
            make_iterator_range(next(begin(src)), end(src))
          , dst
          , --how_many);
      }
  }

  //! Take elements from a range while a predicate holds.

  //! \verbatim
  //! take_while :: (a -> bool) -> [a] ->[a]
  //! take_while _ [] = []
  //! take_while p (x:xs) 
  //!    | p x           = x : take_while f xs
  //!    | otherwise     = []
  //! \endverbatim

  template <class SrcT, class DstT, class PredT>
  void take_while(SrcT const& src, DstT dst, PredT pred)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type;
    //! \tparam PredT Preicate type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \param pred Predicate.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    if(!empty(src) && pred(*begin(src)))
      list::take_while(
          make_iterator_range(next(begin(src)), end(src))
        , *dst++ = *begin(src)
        , pred);
  }
  
  //! Drop the first N elements from a range.

  //! \verbatim
  //! drop :: Int -> [a] -> [a]
  //! drop 0 xs = xs
  //! drop _ [] = []
  //! drop n (x:xs) = drop n-1 xs
  //! \endverbatim

  template <class SrcT, class DstT>
  void drop(SrcT const& src, DstT dst, std::size_t how_many)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \param how_many The number of elements to drop.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    typedef typename boost::range_value<SrcT>::type value_type;

    if(!how_many)
        std::copy(begin(src), end(src), dst);
    else
      if(!empty(src))
        list::drop(
               make_iterator_range(next(begin(src)), end(src))
             , dst
             , --how_many
             );
  }

  //! Drop elements from a range while a predicate holds.

  //! \verbatim
  //! drop_while :: [a] -> (a -> Bool) -> []
  //! drop_while [] f = []
  //! drop_while (x:xs) f
  //!  | f x       = drop_while xs f
  //!  | otherwise = xs
  //! \endverbatim

  template <class SrcT, class DstT, class PredT>
  void drop_while(SrcT const& src, DstT dst, PredT pred)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type;
    //! \tparam PredT Predicate type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \param pred Predicate.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    if(!empty(src))
    {
      if(pred(*begin(src)))
        list::drop_while(
            make_iterator_range(next(begin(src)), end(src))
          , dst
          , pred);
      else
        std::copy(begin(src), end(src), dst);
    }
  }

  //! Return all but the last element of a range.
  
  //! \verbatim
  //! init :: [a] -> [a]
  //! init [] = error
  //! init [_] = []
  //! init (x:xs) = x : init xs
  //! \endverbatim
  
  template <class SrcT, class DstT>
  void init(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \exception std::runtime_error \code null(src) \endcode

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::size;
    using boost::make_iterator_range;

    if(empty(src))
      throw std::runtime_error("empty range");

    if(size(src) != 1)
      list::init(
         make_iterator_range(next(begin(src)), end(src))
      , *dst++ = *begin(src)
      );
  }

  //! Concatenate a range of ranges into a single range.

  //! \verbatim
  //! concat :: [[a]] -> [a]
  //! concat [] = []
  //! concat (x:xs) = x : concat xs
  //! \endverbatim

  template<class SrcT, class DstT>
  void concat(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::size;
    using boost::make_iterator_range;

    if(!empty(src))
    {
      std::copy(begin(*begin(src)), end(*begin(src)), dst);

      list::concat(make_iterator_range(next(begin(src)), end(src)), dst);
    }
  }

  //! Create a range containing N copies of a given value.

  //! \verbatim
  //! replicate :: a -> Int -> [a]
  //! replicate _ 0 = []
  //! replicate x n = x : replicate x n-1
  //! \endverbatim
  
  template <class T, class DstT>
  void replicate(T t, int how_many, DstT dst)
  {
    //! \tparam T Value type;
    //! \tparam DstT Destination iterator type.
    //! \param t value Value to replicate;
    //! \param how_many The number of times to replicate the value;
    //! \param dst Destination iterator.

    if(how_many != 0)
      list::replicate(t, how_many - 1, *dst++ = t);
  }

  //! Select the ith element of a range.
  
  //! \verbatim
  //! select :: [a] -> int -> a
  //! select [] _ = error
  //! select (x:xs) 0 = x
  //! select (x:xs) n = select xs n - 1
  //! \endverbatim

  template <class SrcT>
  typename boost::range_value<SrcT>::type select(SrcT const& src, int i)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range;
    //! \param i Index of the element to select.
    //! \return The range value at the provided index.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::next;
    using boost::make_iterator_range;
    
    if(empty(src))
      throw std::runtime_error("empty range");

    if(i == 0)
      return *(begin(src));
    else
      return list::select(make_iterator_range(next(begin(src)), end(src)), i - 1);
  }

  //! Merge two sorted ranges into a single sorted range.

  //! \verbatim
  //! merge :: Ord a => [a] -> [a] -> [a]
  //! merge [] xs = xs
  //! merge xs [] = xs
  //! merge (x:xs) (y:ys)
  //!  | x < y     =  x : merge xs (y:ys)
  //!  | otherwise =  y : merge (x:xs) ys
  //! \endverbatim

  template <class SrcT, class DstT>
  void merge(SrcT const& first, SrcT const& second, DstT dst)
  {
    //! \tparam SrcT Source range type ;
    //! \tparam DstT Destination iterator type.
    //! \param first One of the two sorted ranges to merge;
    //! \param second The other of the two sorted ranges to merge;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::next;
    using boost::make_iterator_range;
    
    if(empty(first))
      std::copy(begin(second), end(second), dst);
    else if(empty(second))
      std::copy(begin(first), end(first), dst);
    else
      if(*begin(first) <= *begin(second))
        list::merge(
          make_iterator_range(next(begin(first)), end(first))
        , make_iterator_range(begin(second), end(second))
        , *dst++ = *begin(first));
      else
        list::merge(
          make_iterator_range(begin(first), end(first))
        , make_iterator_range(next(begin(second)), end(second))
        , *dst++ = *begin(second));
  }

  //! Sort a range using the "merge sort" algorithm.
  
  //! \verbatim
  //! merge_sort :: Ord a => [a] -> [a]
  //! merge_sort [] = []
  //! merge_sort [x] = [x]
  //! merge_sort xs =
  //!    merge (merge_sort as) (merge_sort bs)
  //!      where (as, bs) = splitAt ((length xs) `div` 2) xs
  //! \endverbatim

  template <class SrcT, class DstT>
  void merge_sort(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type ;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::size;
    using boost::next;
    using boost::make_iterator_range;

    typedef typename boost::range_value<SrcT>::type value_type;

    if(!empty(src))
    {
      std::size_t n = size(src);
      if(n == 1)
        *dst++ = *begin(src);
      else
      {
        std::vector<value_type> left, right;
        list::merge_sort(
            make_iterator_range(begin(src), next(begin(src), n/2))
          , std::back_inserter(left));
        list::merge_sort(
            make_iterator_range(next(begin(src), n/2), end(src))
          , std::back_inserter(right));

        list::merge(left, right, dst);
      }
    }
  }

  //! Test if a value is an element of a range.

  //! \verbatim
  //! elem :: [a] -> a -> bool
  //! elem [] _ = False
  //! elem (x:xs)
  //!  |  x == y      = true
  //!  | otherwise    = elem xs y
  //! \endverbatim

  template <class SrcT>
  bool elem(SrcT const& src, typename boost::range_value<SrcT>::type const& x)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range;
    //! \param x Value to test for membership.
    //! \return True if the value is an element of the range, false otherwise.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::next;
    using boost::make_iterator_range;

    if(empty(src))
      return false;

    return *begin(src) ==
      x ?  true : list::elem(make_iterator_range(next(begin(src)), end(src)), x);
  }

  //! Sum the elements of a range.
  
  //! \verbatim
  //! sum :: [a] -> a
  //! sum [] = 0
  //! sum (x:xs) = x + sum xs
  //! \endverbatim

  template <class SrcT>
  typename boost::range_value<SrcT>::type
  sum(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range;
    //! \return The sum of the elements of the range.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::next;
    using boost::make_iterator_range;

    if(empty(src))
      return 0;
    else
      return *begin(src) +
        list::sum(make_iterator_range(next(begin(src)), end(src)));
  }

  //! Compute the product of the elements of a range.

  //! \verbatim
  //! product :: [a] -> a
  //! product [] = 1
  //! product (x:xs) = x * product xs
  //! \endverbatim
  
  template <class SrcT>
  typename boost::range_value<SrcT>::type
  product(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range;
    //! \return The product of the elements of the range.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::next;
    using boost::make_iterator_range;

    if(empty(src))
      return 1;
    else
      return *begin(src)*list::product(
        make_iterator_range(next(begin(src)), end(src)));
  }

  //! Test if a range is sorted.

  //! \verbatim
  //! sorted :: [a] -> Bool
  //! sorted [] = True
  //! sorted [_] = True
  //! sorted (x:y:ys)
  //!  | x > y           = False
  //!  | otherwise       = sorted ys
  //! \endverbatim

  template <class SrcT>
  bool sorted(SrcT const& src)
  {
    //! \tparam SrcT The range type.
    //! \param src Source range.
    //! \return True if the range is sorted, false otherwise.

    using boost::begin;
    using boost::end;
    using boost::empty;
    using boost::next;
    using boost::size;
    using boost::make_iterator_range;

    if(size(src) < 2)
      return true;
    else
      if(*begin(src) > *next(begin(src)))
        return false;
      else
        return list::sorted(make_iterator_range(next(begin(src), 2), end(src)));
  }

  //! Test for a null range.

  //! \verbatim
  //! null :: [a] -> Bool
  //! null [] = True
  //! null (_:_) = False
  //! \endverbatim

  template <class SrcT>
  inline void is_null(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range.
    //! \return True if the source range is empty otherwise false.

    return boost::empty(src);
  }

  //! Return the head of a range.

  //! \verbatim
  //! head :: [a] -> a
  //! head [] = error
  //! head (x:_) = x
  //! \endverbatim

  template <class SrcT>
  typename boost::range_value<SrcT>::type head(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range.
    //! \return The first element of the source range.
    //! \exception std::runtime_error \code null(src) \endcode

    using boost::begin;
    using boost::empty;

    if(empty(src))
      throw std::runtime_error("empty range");

    return *(begin(src));
  }

  //! Return the tail of a range.

  //! \verbatim
  //! tail :: [a] -> [a]
  //! tail [x] = error
  //! tail (_:xs) = xs
  //! \endverbatim

  template <class SrcT, class DstT>
  void tail(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type ;
    //! \tparam DstT Destianation iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \exception std::runtime_error \code null(src) \endcode

    using boost::begin;
    using boost::next;
    using boost::empty;
    using boost::end;

    if(empty(src))
      throw std::runtime_error("empty range");

    std::copy(next(begin(src)), end(src), dst);
  }

  //! Return the last element of a range.

  //! \verbatim
  //! last :: [a] -> a
  //! last [] = error
  //! last [x] = x
  //! last (x:xs) = last xs
  //! \endverbatim

  template <class SrcT>
  typename boost::range_value<SrcT>::type last(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range.
    //! \return The last element of the source range.
    //! \exception std::runtime_error \code null(src) \endcode

    using boost::begin;
    using boost::next;
    using boost::size;
    using boost::end;
    using boost::empty;
    using boost::make_iterator_range;

    if(empty(src))
      throw std::runtime_error("empty range");

    if(size(src) == 1)
      return *begin(src);

    return list::last(
      make_iterator_range(next(begin(src)), end(src)));
  }

  //! Given a range, gather adjacent elements into pairs.

  //! \verbatim
  //! pairs :: [a] -> [(a, a)]
  //! pairs [] = []
  //! pairs xs = zip xs tail xs
  //! \endverbatim

  template <class SrcT, class DstT>
  void pairs(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type ;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::make_zip_iterator;
    using boost::begin;
    using boost::next;
    using boost::prior;
    using boost::end;
    using boost::empty;
    using boost::make_tuple;
    
    typedef typename boost::range_value<SrcT>::type value_type;

    if(empty(src))
      return;
    else
    {
      std::copy(
         make_zip_iterator(make_tuple(begin(src), next(begin(src))))
       , make_zip_iterator(make_tuple(prior(end(src)), end(src)))
       , dst);
    }
  }

  //! Compute the cartesian product of a pair of ranges.
  
  //! \verbatim
  //! cartesian_product :: [a] -> [a] -> [(a, a)]
  //! cartesian_product [] _ = []
  //! cartesian_product _ [] = []
  //! cartesian_product (x:xs) (y:ys) =
  //!  [(x, y)] ++ (cartesian_product [x] ys) ++ (cartesian_product xs y:ys)
  //! \endverbatim

  template <class Src1T, class Src2T, class DstT>
  void cartesian_product(Src1T const& l, Src2T const& r, DstT dst)
  {
    //! \tparam Src1T First range type;
    //! \tparam Src2T Other range type;
    //! \tparam DstT Destination iterator type.
    //! \param l First range;
    //! \param r Other range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_tuple;
    using boost::make_iterator_range;

    if(empty(l) || empty(r))
      return;

    *dst++ = make_tuple(*begin(l), *begin(r));

    list::cartesian_product(
          make_iterator_range(begin(l), next(begin(l)))
        , make_iterator_range(next(begin(r)), end(r))
        , dst);
    list::cartesian_product(
          make_iterator_range(next(begin(l)), end(l))
        , r
        , dst);
  }

  //! Zip a pair of ranges into a range of pairs.

  //! \verbatim
  //! zip :: [a] -> [a] -> [(a, a)]
  //! zip [][xs] = []
  //! zip [xs] [] = []
  //! zip (x:xs) (y:ys) = [(x, y)] ++ zip xs ys
  //! \endverbatim

  template <class Src1T, class Src2T, class DstT>
  void zip(Src1T const& l, Src2T const& r, DstT dst)
  {
    //! \tparam Src1T First range type;
    //! \tparam Src2T Other range type;
    //! \tparam DstT Destination iterator type.
    //! \param l First range;
    //! \param r Other range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_tuple;
    using boost::make_iterator_range;

    if(empty(l) || empty(r))
      return;
    else
      list::zip(
          make_iterator_range(next(begin(l)), end(l))
        , make_iterator_range(next(begin(r)), end(r))
        , *dst++ = make_tuple(*begin(l), *begin(r))
        );
  }

  //! Apply a function to each pair resulting from the zipping of two ranges.

  //! \verbatim
  //! zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
  //! zip_with _ [] [_] = []
  //! zip_with _ [_] [] = []
  //! zip_with f (x:xs) (y:ys) = f x y : zip_with f xs ys
  //! \endverbatim

  template <class F, class Rg1T, class Rg2T, class DstT>
  void zip_with(F f, Rg1T const& l, Rg2T const& r, DstT dst)
  {
    //! \tparam F Function (object) type;
    //! \tparam Src1T First range type;
    //! \tparam Src2T Other range type;
    //! \tparam DstT Destination iterator type.
    //! \param f Function to apply;
    //! \param l First range;
    //! \param r Other range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    if(empty(l) || empty(r))
      return;
    else
    {
      list::zip_with(
          f
        , make_iterator_range(next(begin(l)), end(l))
        , make_iterator_range(next(begin(r)), end(r))
        , *dst++ = f(*begin(l), *begin(r)));
    }
  }

  //! Compute a pair of ranges from a range of pairs.

  //! \verbatim
  //! unzip :: [(a, b)] -> ([a], [b])
  //! unzip [] = ([], [])
  //! unzip (x: xs) =
  //!   let
  //!     (a, b) = x
  //!     (as, bs) = unzip xs
  //!   in
  //!     (a:as, b:bs)
  //! \endverbatim

  template <class SrcT, class Dst1T, class Dst2T>
  void unzip(SrcT const& src, Dst1T left, Dst2T right)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param left Left destination iterator;
    //! \param right Right destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    if(empty(src))
      return;

    list::unzip(
        make_iterator_range(next(begin(src)), end(src))
      , *left++ = begin(src)->template get<0>()
      , *right++ = begin(src)->template get<1>());
  }
  
  //! Find the minimum value in a range.
  
  //! \verbatim
  //! minimum : [a] -> a
  //! minimum [] = error
  //! minuimum [x] = x
  //! minimum (x:xs) = min x (minimum xs)
  //! \endverbatim

  template <class SrcT>
  typename boost::range_value<SrcT>::type minimum(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range.
    //! \return The minimum element of the range.
    //! \exception std::runtime_error \code null(src) \endcode
    
    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::size;
    using boost::make_iterator_range;

    if(empty(src))
      throw std::runtime_error("empty range");

    if(size(src) == 1)
      return *begin(src);

    return (std::min)(
              *begin(src)
            , list::minimum(
                make_iterator_range(next(begin(src)), end(src))));
  }

  //! Find the maximum value of a range.

  //! \verbatim
  //! maximum :: [a] -> a
  //! maximum [] = error
  //! minuimum [x] = x
  //! maximum (x:xs) = max x maximum xs
  //! \endverbatim

  template <class SrcT>
  typename boost::range_value<SrcT>::type maximum(SrcT const& src)
  {
    //! \tparam SrcT Source range type .
    //! \param src Source range.
    //! \return The maximum element of the range.
    //! \exception std::runtime_error \code null(src) \endcode

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::size;
    using boost::make_iterator_range;

    if(empty(src))
      throw std::runtime_error("empty range");

    if(size(src) == 1)
      return *begin(src);

    return (std::max)(
              *begin(src)
            , list::maximum(
                make_iterator_range(next(begin(src)), end(src))));
  }

  //! Returns a range of successively reduced values from the left.

  //! \verbatim
  //! scanl :: (a -> [b] -> a) -> a -> [b] -> [a]
  //! scanl f x [] = x : []
  //! scanl f x (y:ys) = x : scanl f (f x y) ys
  //! \endverbatim

  template <class F, class SrcT, class DstT>
  void scanl(
    F f, typename boost::range_value<SrcT>::type x, SrcT src, DstT dst)
  {
    //! \tparam F Function (object) type;
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param f Function to apply;
    //! \param x Starting value;
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    typedef typename boost::range_value<SrcT>::type value_type;

    if(empty(src))
      *dst++ = x;
    else
    {
      list::scanl(
          f
        , f(x, *begin(src))
        , make_iterator_range(next(begin(src)), end(src))
        , *dst++ = x
        );
    }
  }

  //! scanl1 is similiar to scanl but without the starting element.
  
  //! \verbatim
  //! scanl1 :: (a -> a -> a) -> [a] -> [a]
  //! scanl1 _ [] = []
  //! scanl1 f (x:xs) = scanl f x xs
  //! \endverbatim

  template <class F, class SrcT, class DstT>
  void scanl1(F f, SrcT src, DstT dst)
  {
    //! \tparam F Function (object) type;
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param f Function to apply;
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    if(empty(src))
      return;
    else
      list::scanl(
          f
        , *begin(src)
        , make_iterator_range(next(begin(src)), end(src))
        , dst
      );
  }

  //! split_at xs n = (take xs n, drop xs n)

  template <class SrcT, class DstT>
  inline void split_at(SrcT const& src, DstT left, DstT right, int n)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param left Left destination iterator;
    //! \param right Right destination iterator;
    //! \param n The index at which to split.

    list::take(src, left, n);
    list::drop(src, right, n);
  }

  //! span p xs = (take_while p xs, drop_while p xs)

  template <class SrcT, class Dst1T, class Dst2T, class PredT>
  inline void span(SrcT const& src, Dst1T left, Dst2T right, PredT pred)
  {
    //! \tparam SrcT Source range type;
    //! \tparam Dst1T Left destination iterator type;
    //! \tparam Dst2T Right destination iterator type;
    //! \tparam PredT Predicate type.
    //! \param src Source range;
    //! \param left Left destination iterator;
    //! \param right Right destination iterator;
    //! \param pred Predicate.

    list::take_while(src, left, pred);
    list::drop_while(src, right, pred);
  }

  //! break p xs = span not.p xs

  template <class SrcT, class Dst1T, class Dst2T, class PredT>
  inline void break_(SrcT const& src, Dst1T left, Dst2T right, PredT pred)
  {
    //! \tparam SrcT Source range type;
    //! \tparam Dst1T Left destination iterator type;
    //! \tparam Dst2T Right destination iterator type;
    //! \tparam PredT Predicate type.
    //! \param src Source range;
    //! \param left Left destination iterator;
    //! \param right Right destination iterator;
    //! \param pred Predicate.

    span(src, left, right, std::not1(pred));
  }

  //! Break a string up into a range of strings at newline characters.

  //! \verbatim
  //! lines            :: String -> [String]
  //! lines ""         =  []
  //! lines s          =  let (l, s') = break (== '\n') s
  //!                       in  l : case s' of
  //!                                 []      -> []
  //!                                 (_:s'') -> lines s''
  //! \endverbatim

  template <class SrcT, class DstT>
  void lines(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;
    
    using boost::phoenix::arg_names::arg1;

    if(!empty(src))
    {
      std::string left, right;
      list::span(src, std::back_inserter(left), std::back_inserter(right), arg1 != '\n');
      *dst++ = left;
      list::lines(make_iterator_range(++right.begin(), right.end()), dst); //newlines are not included
    }
  }
  
  //! Break a string into a range of words.
  
  //! \verbatim
  //! words            :: String -> [String]
  //! words s          =  case dropWhile Char.isSpace s of
  //!                       "" -> []
  //!                       s' -> w : words s''
  //!                             where (w, s'') = break Char.isSpace s
  //! \endverbatim

  template <class SrcT, class DstT>
  void words(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    std::string s;

    list::drop_while(
        src
      , std::back_inserter(s)
      , static_cast<int(*)(int)>(std::isspace));

    if(!empty(s))
    {
      std::string l, r;
      break_(
          s
        , std::back_inserter(l)
        , std::back_inserter(r)
        , std::ptr_fun(static_cast<int(*)(int)>(std::isspace))
        );
      list::words(r, *dst++ = l);
    }
      
  }

  //! Append a range to a range.

  //! \verbatim
  //! (++) :: [a] -> [a] -> [a]
  //! []     ++ ys = ys
  //! (x:xs) ++ ys = x : (xs ++ ys)
  //! \endverbatim

  template <class Src1T, class Src2T, class DstT>
  void append(Src1T const& src1, Src2T const& src2, DstT dst)
  {
    //! \tparam Src1T First range type;
    //! \tparam Src2T Other range type;
    //! \tparam DstT Destination iterator type.
    //! \param src1 First range;
    //! \param src2 Other range;
    //! \param dst Destination iterator.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    if(empty(src1))
    {
      std::copy(begin(src2), end(src2), dst);
    }
    else
    {
      *dst++ = *begin(src1);
      list::append(make_iterator_range(next(begin(src1)), end(src1)), src2, dst);
    }
  }

  //! Find all subranges of a given length.

  //! \verbatim
  //! subrange :: [a] -> Int -> [[a]]
  //! subrange [] _ = []
  //! subrange [xs] 1 = [[x] | x <- xs]
  //! subrange (x:xs) n
  //!    | n > len (x:xs) = []
  //!    | otherwise = [x : y | y <- subrange xs n - 1] ++ subrange xs n
  //! \endverbatim

  template <class SrcT, class DstT>
  void subrange(SrcT const& src, DstT dst, int n)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator;
    //! \param n Length of the subrange.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::size;
    using boost::make_iterator_range;

    typedef typename boost::range_value<SrcT>::type value_type;
    typedef std::vector<value_type> list_type;

    if(empty(src) || n > size(src))
      return;

    if(n == 1)
    {
      BOOST_FOREACH(value_type const& x, src)
      {
        list_type t;
        list::append(list_type(1, x), list_type(), std::back_inserter(t));
  
        *dst++ = t;
      }
    }
    else
    {
      std::vector<list_type> tmp;
      list::subrange(
          make_iterator_range(
            next(begin(src)), end(src)
            )
        , std::back_inserter(tmp), n - 1
        );
  
      BOOST_FOREACH(list_type const& x, tmp)
      {
        list_type t;
        list::append(list_type(1, *begin(src)), x, std::back_inserter(t));
  
        *dst++ = t;
      }
  
      list::subrange(make_iterator_range(next(begin(src)), end(src)), dst, n);
    }
  }
  
  namespace detail //Helper for subsets().
  {
    template <class SrcT, class DstT>
    inline void subsets_impl(SrcT const& src, DstT dst, std::size_t n)
    {
      if(n != 0)
      {
        list::subrange(src, dst, n);
        list::detail::subsets_impl(src, dst, n-1);
      }
    }

  }//namespace detail

  //! Find all non-empty subsets of a range.

  //! \verbatim
  //! subsets :: [a] -> [[a]]
  //! subsets [] = []
  //! subsets [xs] = subsets_impl xs (length xs)
  //!    where
  //!     subsets_impl [] _ = []
  //!     subsets_impl xs n = subrange xs n ++ (subsets_impl xs n-1)
  //! \endverbatim

  template <class SrcT, class DstT>
  void subsets(SrcT const& src, DstT dst)
  {
    //! \tparam SrcT Source range type;
    //! \tparam DstT Destination iterator type.
    //! \param src Source range;
    //! \param dst Destination iterator.

    using boost::empty;
    using boost::size;

    if(!empty(src))
      list::detail::subsets_impl(src, dst, size(src));
  }

  //! Fold left.

  //! \verbatim
  //! fold :: (a -> b -> a) -> [b] -> [a]
  //! fold f z [] = z
  //! fold f z (x:xs) = fold (f z x) xs
  //! \endverbatim

  template <class AccT, class SrcT, class F>
  AccT foldl(SrcT const& src, AccT z, F f)
  {
    //! \tparam AccT Accumulator type;
    //! \tparam SrcT Source range type;
    //! \tparam F Function (object) type.
    //! \param src Source range;
    //! \param z Initial value;
    //! \param f Function object.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::size;
    using boost::make_iterator_range;

    if(empty(src))
      return z;

    return list::foldl(
        make_iterator_range(
          next(begin(src)), end(src)
          )
      , f(z, *begin(src))
      , f
      );
  }
  
  //! The right to left dual of foldl.

  //! \verbatim
  //! foldr            :: (a -> b -> b) -> b -> [a] -> b
  //! foldr f z []     =  z
  //! foldr f z (x:xs) =  f x (foldr f z xs)
  //! \endverbatim

  template <class AccT, class SrcT, class F>
  AccT foldr(SrcT const& src, AccT z, F f)
  {
    //! \tparam AccT Accumulator type;
    //! \tparam SrcT Source range type;
    //! \tparam F Function (object) type.
    //! \param src Source range;
    //! \param z Initial value;
    //! \param f Function object.

    using boost::begin;
    using boost::end;
    using boost::next;
    using boost::empty;
    using boost::make_iterator_range;

    if(empty(src))
      return z;

    return f(
        *begin(src)
      , list::foldr(
            make_iterator_range(
              next(begin(src)), end(src)
              )
          , z
          , f
        )
      );
  }

  //! The conjuction of a range of bool.

  //! \verbatim
  //! and :: [Bool] -> Bool
  //! and               =  foldr (&&) True
  //! \endverbatim

  template <class SrcT>
  inline bool and_(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range.
    //! \return Conjuction of the source range.
    
    using boost::phoenix::arg_names::arg1;
    using boost::phoenix::arg_names::arg2;

    return list::foldr(src, true, std::logical_and<bool>());
  }

  //! The disjunctive dual of 'and'.

  //! \verbatim
  //! or :: [Bool] -> Bool
  //! or               =  foldr (||) False
  //! \endverbatim

  template <class SrcT>
  inline bool or_(SrcT const& src)
  {
    //! \tparam SrcT Source range type.
    //! \param src Source range.
    //! \return Disjunction of the source range.

    return list::foldr(src, false, std::logical_or<bool>());
  }

  //! Do all the elements of a range satisfy a predicate?

  //! \verbatim
  //! all :: (a -> Bool) -> [a] -> Bool
  //! all = and.map p
  //! \endverbatim

  template <class SrcT, class PredT>
  bool all(SrcT const& src, PredT pred)
  {
    //! \tparam SrcT Source range type;
    //! \tparam PredT Predicate type.
    //! \param src Source range;
    //! \param pred Predicate.
    //! \return True if the predicate holds for all elements of the source
    //!         range and false otherwise.

    std::vector<bool> tmp;
    list::map(src, std::back_inserter(tmp), pred);

    return and_(tmp);
  }
  
  //! Do any of the elements of a range satisfy a predicate?

  //! \verbatim
  //! any :: (a -> Bool) -> [a] -> Bool
  //! any = and.map p
  //! \endverbatim

  template <class SrcT, class PredT>
  bool any(SrcT const& src, PredT pred)
  {
    //! \tparam SrcT Source range type;
    //! \tparam PredT Predicate type.
    //! \param src Source range;
    //! \param pred Predicate.
    //! \return True if the predicate holds for any element of the source
    //!         range and false otherwise.

    std::vector<bool> tmp;
    list::map(src, std::back_inserter(tmp), pred);

    return or_(tmp);
  }

}//namespace list

#endif//!defined(LIST_1CD56401_BB89_4D12_9D83_1A17B0A647C3_INCLUDED)
 
