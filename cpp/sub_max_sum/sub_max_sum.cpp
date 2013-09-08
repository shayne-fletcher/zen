#include <vector>
#include <numeric>
#include <algorithm>
#include <iterator>
#include <iostream>
#include <functional>

typedef std::pair<std::size_t, std::size_t> indicies_t;
inline std::size_t& fst (indicies_t& i) { return i.first; }
inline std::size_t& snd (indicies_t& i) { return i.second; }
inline std::size_t fst (indicies_t const& i) { return i.first; }
inline std::size_t snd (indicies_t const& i) { return i.second; }
void print_indicies (indicies_t const& p);

typedef std::vector<int> interval_t;
inline std::size_t len (interval_t const& i) { return i.size(); }
interval_t take (int k, interval_t const& lst);
interval_t drop (int k, interval_t const& lst);
inline int sum (interval_t const t)
{ 
  int tot=std::accumulate (t.begin(), t.end(), 0, std::plus<int>());

  return tot;
}
interval_t range (int begin, int end);
void print_interval (interval_t const& s);

struct interval_info_t
{
  indicies_t indicies; //(0, 2)
  interval_t interval; //e.g. [1, 2, 3]
  int total; //6
};
void print_interval_info (interval_info_t const& i);

typedef std::vector<interval_info_t> interval_info_set_t;

struct inner_f
{
  interval_t s;
  std::size_t i;

  inner_f (interval_t const& s, std::size_t i) 
    : s (s), i (i) 
  {}

  interval_info_set_t operator() 
  (interval_info_set_t acc, std::size_t j)
  {
    indicies_t indicies;
    fst (indicies) = i;
    snd (indicies) = i + j - 1;
    interval_t interval = take (j, s);
    int total = sum (interval);

    interval_info_t interval_info;
    interval_info.indicies = indicies;
    interval_info.interval = interval;
    interval_info.total = total;

    acc.push_back (interval_info);
  
    return acc;
 }
};

interval_info_set_t row (std::size_t i, interval_t const& t)
{
  interval_t s = drop (i, t);
  interval_t rg = range (1, len (s) + 1);
  
  return std::accumulate (
    rg.begin(), rg.end(), interval_info_set_t(), inner_f (s, i));
}

struct outer_f
{
  interval_t t;
  outer_f (interval_t const t) : t (t) {}

  interval_info_set_t operator() 
    (interval_info_set_t acc, std::size_t i) const
  {
    interval_info_set_t res=row (i, t);
    std::copy (res.begin(), res.end(), std::back_inserter (acc));
  
    return acc;
  }
};

interval_info_set_t intervals (interval_t const& t)
{
  interval_t rg = range (0, len (t));
  
  return std::accumulate (
    rg.begin(), rg.end(), interval_info_set_t (), outer_f (t));
}

inline int find_max (int m, interval_info_t const& t)
{
  return (std::max)(m, t.total);
}

//--driver

int main ()
{
  int data[] = {1, 3, -8, 2, -1, 10, -2, 1};
  interval_t input(data, data + sizeof(data)/sizeof(int));
  interval_info_set_t ivals = intervals (input);
  int maxsum = std::accumulate (ivals.begin(), ivals.end(), 0, find_max);

  for (std::size_t i = 0; i < ivals.size(); ++i)
    {
      if (ivals[i].total==maxsum)
	print_interval_info (ivals[i]); //An interval of maximal sum.
    }
  
  return 0;
}

//-- tools

interval_t take (int k, interval_t const& lst)
{
  if (k <= 0) return interval_t ();
  if (lst.size() == std::size_t (0)) return interval_t ();

  interval_t t = take (k-1, interval_t (++(lst.begin()), lst.end()));
  interval_t res (t.size()+1);
  res[0] = lst[0];
  for (std::size_t i = 0; i < len (t); ++i)
    res [i+1] = t[i];

  return res;
}

interval_t drop (int k, interval_t const& lst)
{
  if (k <= 0) return lst;
  if (lst.size () == 0) return interval_t ();

  return drop (k - 1, interval_t (++(lst.begin()), lst.end()));
}

interval_t range (int begin, int end)
{
  interval_t buf;
  for(int i = begin; i < end; ++i) buf.push_back (i);

  return buf;
}

void print_indicies (indicies_t const& p)
{
  std::cout << "(" << fst (p) << ", " << snd (p) << ")";
}

void print_interval (interval_t const& s)
{
  std::cout << "["; 
  std::ostream_iterator<int> out_it (std::cout, ", ");
  std::copy (s.begin(), s.end(), out_it);
  std::cout << "],"; 
}

void print_interval_info (interval_info_t const& i)
{
  std::cout << "(";
  print_indicies (i.indicies);
  print_interval (i.interval);
  std::cout << i.total;
  std::cout << ")\n";
}
