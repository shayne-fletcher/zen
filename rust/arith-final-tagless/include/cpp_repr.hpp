#pragma once

#include <memory>
#include <cstdint>

struct Cpp_repr {
  int64_t expr;
};

using cpp_repr_t = std::shared_ptr<Cpp_repr>;

cpp_repr_t lit(int64_t i);
cpp_repr_t neg(cpp_repr_t t);
cpp_repr_t add(cpp_repr_t t, cpp_repr_t u);
