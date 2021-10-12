#include <iostream>
#include <cstdlib>

#include "rust/cxx.h" // 'rust::Error'
#include "multi-cxx-exp/src/multi_cxx1.rs.h"
#include "multi-cxx-exp/src/multi_cxx2.rs.h"

struct thing {
  std::int64_t id;
};

struct multi_cxx {
  thing thing;
};

namespace {
  template <class T> using Box = rust::Box<T>;
}//namespace<anonymous>

int main() {

  Box<MultiCxx1Thing> p1 = new_thing(1);
  Box<MultiCxx2Thing> p2 = another_new_thing(2);

  thing const& thing1 = ((multi_cxx const&)(*p1)).thing;
  std::cout << "I am thing " << thing1.id << std::endl;
  thing const& thing2 = ((multi_cxx const&)(*p2)).thing;
  std::cout << "I am thing " << thing2.id << std::endl;

  print_thing_ref((Box<MultiCxx1Thing> const&)(*(&p2)));
  another_print_thing_ref((Box<MultiCxx2Thing> const&)(*(&p1)));

  print_thing(std::move((Box<MultiCxx1Thing>&)(*(&p2))));
  another_print_thing(std::move((Box<MultiCxx2Thing>&)(*(&p1))));

  return 0;
}
