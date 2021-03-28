#include <iostream>

#include "rust/cxx.h"
#include "arith_final_tagless/src/arith_final_tagless.rs.h"

int main() {
  try {
    std::string s{"8 + -(1 + 2)"};
    if (auto p = parse_cpp(::rust::String{s})) {
      std::cout << s << " = " << p->expr << '\n';
    }
  } catch (rust::Error const& e) {
    std::cerr << e.what() << std::endl;
  }

  return 0;
}
