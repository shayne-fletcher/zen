#include <iostream>
#include <iomanip>
#include <cstdint>

#include "arith.h"

std::int64_t eval_term(arith::Term const* t);

int main() {
  char const* prompt = "\n% ";
  std::cout << "Additive expression evalutator (Ctrl+D to exit)" << prompt;

  std::string line;
  while(std::getline(std::cin, line)) {
    try {
      auto alloc = arith::parse_ffi_alloc();
      if (auto p = arith::parse_ffi(alloc, line.c_str())) {
        std::cout << line << " = " << eval_term(p) << prompt;
      }
      arith::parse_ffi_free_alloc(alloc);
    } catch (...) {
      std::cerr << "An unidentifiable exception occured" << std::endl;
    }
  }

  return 0;
}

std::int64_t eval_term(arith::Term const* t) {
  using namespace arith;

  switch(t -> tag) {
  case Term::Tag::Lit:{
    return t->lit._0;
  }
  case Term::Tag::Neg:{
    return -eval_term(t->neg._0);
  }
  case Term::Tag::Add:{
    return eval_term(t->add._0) + eval_term(t->add._1);
  }
  }
}
