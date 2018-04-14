//g++ -std=c++11 -o collinear collinear.cpp

#include <iostream>

bool collinear (int x1, int x2, int x3, int y1, int y2, int y3) {
  return (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) == 0;
}

struct point {int x; int y;};

bool operator < (point const& p1, point const& p2) {
  if (p1.x < p2.x) return true;
  if (p1.x > p2.x) return false;

  return (p1.y < p2.y);
}

template <class ItT>
bool is_quadrilateral (ItT begin, ItT end) {
  std::sort (begin, end);
  bool acc = true;
  do
  {
    point p1 = begin[0];
    point p2 = begin[1];
    point p3 = begin[2];
    acc = acc && !collinear (p1.x, p2.x, p3.x, p1.y, p2.y, p3.y);
  } while (std::next_permutation (begin, end));

  return acc;
}

int main () {
  point ps[] = {{1, 1}, {2, 2}, {1, 3}, {4, 5}};
  std::cout << std::boolalpha << is_quadrilateral (ps, ps + 4) << std::endl;;

  return 0;
}
