#include <iostream>
#include <iterator>
#include <algorithm>
#include <vector>

typedef std::vector<int> perm_t;
typedef std::vector<int>::iterator perm_iterator_t;
typedef std::vector<int>::const_iterator perm_const_iterator_t;

typedef std::vector<perm_t> perm_set_t;
typedef std::vector<perm_t>::iterator perm_set_iterator_t;
typedef std::vector<perm_t>::const_iterator perm_set_const_iterator_t;

//-- print functions

void print_perm (perm_t const& s)
{
  std::cout << "["; 
  std::ostream_iterator<int> out_it (std::cout, ", ");
  std::copy (s.begin(), s.end(), out_it);
  std::cout << "],"; 
}

void print_perm_set (perm_set_t const& ts)
{
  std::cout << "[";
  perm_set_const_iterator_t begin=ts.begin (), end = ts.end ();
  perm_set_const_iterator_t t=begin;
  while (t != end)
    {
      print_perm (*t);
      ++t;
    }
  std::cout << "]";
}

//-- take, drop

perm_t take (int k, perm_t const& lst)
{
  if (k <= 0)
    {
      return perm_t ();
    }
  if (lst.size() == std::size_t (0))
    {
      return perm_t ();
    }

  perm_t t = take (k-1, perm_t (++(lst.begin()), lst.end()));
  perm_t res (t.size()+1);
  res[0] = lst[0];
  for (std::size_t i = 0; i < t.size (); ++i)
    res [i+1] = t[i];

  return res;
}

perm_t drop (int k, perm_t const& lst)
{
  if (k <= 0)
    {
      return lst;
    }

  if (lst.size () == 0)
    {
      return perm_t ();
    }

  return drop (k - 1, perm_t (++(lst.begin()), lst.end()));
}

//-- helpers

perm_set_t find_all_permutations (perm_t const& lst);

//Prepend x to a perm.

struct prepend
{
  int x;

  prepend (int x) : x (x)
  {}

  perm_t operator ()(perm_t const& p) const
  {
    perm_t r (p.size() + 1);
    r[0] = x;
    for (std::size_t i = 0; i < p.size(); ++i)
      r[i + 1] = p[i];

    return r;
  }
};

//Find all permutations of the given list that start with the element
//at index k.

perm_set_t find_k_permutations (int k, perm_t const& lst)
{
  int x = lst[k];
  perm_t left = take (k, lst);
  perm_t right = drop (k + 1, lst);
  perm_t listp;
  std::copy (left.begin(), left.end(), std::back_inserter (listp));
  std::copy (right.begin(), right.end(), std::back_inserter (listp));
  perm_set_t ps = find_all_permutations (listp);
  perm_set_t s;
  std::transform (ps.begin(), ps.end(), std::back_inserter(s), prepend (x));

  return s;
}

perm_set_t loop (int k, perm_set_t acc, perm_t lst)
{
  if (k == lst.size())
    return acc;
  else
    {
      perm_set_t t = find_k_permutations (k, lst);
      std::copy (t.begin (), t.end (), std::back_inserter (acc));
      
      return loop (k+1, acc, lst);
    }
}

perm_set_t find_all_permutations (perm_t const& lst)
{
  if (lst.size() == 0)
    {
      perm_set_t res;
      res.push_back (perm_t ());//The empty sequence.

      return res;
    }

  return loop (0, perm_set_t (), lst);
}

//-- test

int main()
{
  int data[] = {1, 2, 3};
  perm_t input (data, data + sizeof(data)/sizeof(int));

  print_perm_set (find_all_permutations (input));

  return 0;
}
