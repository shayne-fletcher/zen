#include <algorithm>
#include <iterator>
#include <iostream>
#include <vector>
#include <chrono>
#include <cstdint>
#include <numeric>

using int_t = std::int64_t;
using vector_t = std::vector<int_t>;

vector_t sieve (int_t n) {
  vector_t factors;

  int_t i = 3;
  int_t stop{ int_t (std::floor (std::sqrt (n)))};
  while (i < stop) {
    if (i * i > n) break;
    while (n % i == 0) {
      factors.push_back (i);
      n /= i;
    }
    ++i;
  }
  if (n > 1) 
    factors.push_back (n);

  return factors;
}

int main () {

  using std::chrono::high_resolution_clock;
  using std::chrono::duration_cast;
  using std::chrono::nanoseconds;

  int_t n {UINT64_C(600851475143)};

  auto t = high_resolution_clock::now ();

  //Find the prime factors of `n` by trial division
  vector_t factors = sieve (n);

  auto u = high_resolution_clock::now ();

  auto elapsed = duration_cast<nanoseconds>(u - t).count ();
  std::cout << elapsed/1.0e9 << "(s)" << std::endl;

  std::copy(factors.begin (), factors.end (), 
     std::ostream_iterator<std::int64_t>(std::cout, " "));
  std::cout << std::endl;

  return 0;
}
