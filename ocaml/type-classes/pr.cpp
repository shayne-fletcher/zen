#include <string>
#include <iostream>
#include <vector>
#include <iterator>

template <class A> struct Show {};

template <class A>
void print (A const& a) {
  std::cout << Show<A>::show (a) << std::endl;
}

template <>
struct Show<int> {
  static std::string (*show)(int);
};
std::string(*Show<int>::show)(int) = &std::to_string;

template <class A>
struct Show<std::vector<A>> {
  static std::string show (std::vector<A> const& ls);
};

template <class A>
std::string Show<std::vector<A>>::show (std::vector<A> const& ls) {
  bool first=true;
  typename std::vector<A>::const_iterator begin=ls.begin (), end=ls.end ();
  std::string s="[";
  while (begin != end) {
    if (first) first = false;
    else s += ", ";
    s += Show<A>::show (*begin++);
  }
  s += "]";

  return s;
}

namespace detail {

  template<class A, class ItT>
  ItT replicate (int n, A x, ItT dst) {
    if (n <= 0) return dst;
    return replicate ((n - 1), x, *dst++ = x);
  }

}//namespace detail


#if 0 //This code sends at least msvc-14.0 and clang-3.6.2 into
      //unbounded recursion!

template <class A>
void print_nested (int n, A const& x) {
  if (n == 0)
    print (x);
  else {
    std::vector<A> buf;
    detail::replicate(n, x, std::back_inserter(buf));
    print_nested (n - 1, buf);
  }
}

void test_nested () {
  int n;
  std::cin >> n;
  print_nested (n, 5);
}

#endif//if 0

//Test

int main () {
  
  return 0;
}
