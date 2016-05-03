#include <algorithm>
#include <iterator>
#include <iostream>
#include <vector>
#include <chrono>
#include <cstdint>
#include <numeric>

using int_t = std::int64_t;
using vector_t = std::vector<int_t>;

template <class ItT>
bool exists_factor (ItT begin, ItT end, int i) {
  return std::accumulate(begin, end, false,
    [i](bool acc, int_t k) { return acc || i % k== 0; } );
}

void sieve (std::int64_t n, std::vector<int_t>& factors) {
  std::vector<int> primes;
  primes.reserve (10); //try to keep to one allocation
  primes.push_back (2);
  int_t i = 3;
  int_t stop{ int_t (std::floor (std::sqrt (n)))};
  while (i < stop) {
    if (!exists_factor (primes.begin (), primes.end (), i)) {
      int_t p = i;
      primes.push_back (p);
      if (p * p > n) {
        break;
      }
      else {
        while (n % p == 0) {
          factors.push_back (p);
          n /= p;
        }
      }
    }
    ++i;
  }

  if (n > 1) 
    factors.push_back (n);
}

int main () {
  using std::chrono::high_resolution_clock;
  using std::chrono::duration_cast;
  using std::chrono::nanoseconds;

  int_t n {UINT64_C(600851475143)};
  vector_t factors;

  auto t = high_resolution_clock::now ();

  //Find the prime factors of `n` by trial division
  sieve (n, factors);

  auto u = high_resolution_clock::now ();

  auto elapsed = duration_cast<nanoseconds>(u - t).count ();
  std::cout << elapsed/1.0e9 << "(s)" << std::endl;

  std::copy(factors.begin (), factors.end (), 
     std::ostream_iterator<std::int64_t>(std::cout, " "));
  std::cout << std::endl;

  return 0;
}
