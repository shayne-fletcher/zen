/*
Find a longest increasing subsequence
http://basicalgos.blogspot.com/2012/03/37-longest-increasing-sequence-lis.html
 */
#include <iostream>
#include <vector>
#include <iterator>
#include <algorithm>
#include <numeric>

typedef std::vector<int> ics_t;
typedef ics_t::const_iterator ics_const_iterator_t;
typedef ics_t::iterator ics_iterator_t;

typedef std::vector<ics_t> ics_set_t;
typedef ics_set_t::const_iterator ics_set_const_iterator_t;
typedef ics_set_t::iterator ics_set_iterator_t;

//-- print functions

void print_ics (ics_t const& s)
{
  std::cout << "["; 
  std::ostream_iterator<int> out_it (std::cout, ", ");
  std::copy (s.begin(), s.end(), out_it);
  std::cout << "],"; 
}

void print_ics_set (ics_set_t const& ts)
{
  std::cout << "[";
  ics_set_const_iterator_t begin=ts.begin (), end = ts.end ();
  ics_set_const_iterator_t t=begin;
  while (t != end)
    {
      print_ics (*t);
      ++t;
    }
  std::cout << "]";
}

//-- Calculate the length of an ics

std::size_t len_ics (ics_t const& s)
{
  return s.size ();
}

//-- Calculate the max ics length in a set

std::size_t max_ics_len (ics_set_t const& s)
{
  std::vector<std::size_t> lens;
  std::transform (
    s.begin (), s.end (), std::back_inserter (lens), len_ics);

  return std::accumulate (
        lens.begin (), lens.end (), 0, std::max<std::size_t>);
}

//  Extend ts with any new subsequences that can be formed by
//  prepending s[0] to t for t in ts.

struct f // f : ics_set_t * ics_t -> ics_set_t
{
  int s0; 

  f (int s0) : s0 (s0) 
  {}
  ics_set_t operator()(ics_set_t acc, ics_t const& t) const
  {
    if (s0 < t[0])
      {
	ics_t nt (t.size() + 1);
	nt[0] = s0;
	for (std::size_t i=0; i < t.size(); ++i)
	  nt[i+1] = t[i];
	acc.push_back (nt);

	return acc;
      }
    else
      {
	return acc;
      }
  }
};

//Find the set of increasing subsequences of s.
ics_set_t ics (ics_t s)
{
  if (s.size () == 1)
    {
      ics_set_t res;
      res.push_back (s); //s itself is an ics of length 1

      return res;
    }

  //This is the key point.
  ics_set_t ts = ics (ics_t(++(s.begin()), s.end())); 
  //ts are the ics values of s - it's first element

  //Extend ts with any new subsequences that can be formed by
  //prepending s[0] to t for t in ts.

  return std::accumulate (ts.begin (), ts.end(), ts, f(s[0]));
}

int main ()
{
  int data[] = 
  {
    0, 8, 4, 12, 2, 10, 6, 14, 1,9, 5, 13, 3, 11, 7, 15
  };
  ics_t s (data, data + sizeof(data)/sizeof(int));

  ics_set_t res = ics (s);
  std::size_t n=max_ics_len (res);
  for (std::size_t i = 0; i < res.size (); ++i)
    {
      ics_t const& x=res[i];
      if (x.size () == n)
	{
	  print_ics (x);
	  std::cout << std::endl;
	}
    }
  
  return 0;
}
