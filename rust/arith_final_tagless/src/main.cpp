#include <iostream>

#include "rust/cxx.h" // 'rust::Error'
#include "arith_final_tagless/src/arith_final_tagless.rs.h" // 'parse'

int main() {
  char const* prompt = "\n% ";
  std::cout << "Additive expression evalutator (Ctrl+D to exit)" << prompt;

  std::string line;
  while(std::getline(std::cin, line)) {
    try {
      if (auto p = parse(rust::String{line})) {
        std::cout << line << " = " << p->expr << prompt;
      }
    } catch (rust::Error const& e) {
      std::cerr << e.what() << prompt;
    }
  }

  return 0;
}
