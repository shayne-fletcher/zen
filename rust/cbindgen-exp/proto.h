#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

template<typename T = void>
struct Box;

struct Term {
  enum class Tag {
    Lit,
    Neg,
    Add,
  };

  struct Lit_Body {
    int64_t _0;
  };

  struct Neg_Body {
    Box<Term> _0;
  };

  struct Add_Body {
    Box<Term> _0;
    Box<Term> _1;
  };

  Tag tag;
  union {
    Lit_Body lit;
    Neg_Body neg;
    Add_Body add;
  };
};

extern "C" {

Term *parse_ffi(const char *s);

void parse_free_term_ffi(Term *t);

} // extern "C"
