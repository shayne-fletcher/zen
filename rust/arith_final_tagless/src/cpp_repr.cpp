#include <iostream>

#include "arith_final_tagless/include/cpp_repr.hpp"

cpp_repr_t lit(int64_t i) {
  return std::shared_ptr<Cpp_repr>{ new Cpp_repr{i} };
}

cpp_repr_t neg(cpp_repr_t t) {
  return std::shared_ptr<Cpp_repr>{new Cpp_repr{-t->expr}};
}

cpp_repr_t add(cpp_repr_t t, cpp_repr_t u) {
  return std::shared_ptr<Cpp_repr>{new Cpp_repr{t->expr + u->expr}};
}
