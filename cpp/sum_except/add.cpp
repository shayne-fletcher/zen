//The C++ `try...catch` construct allows for discrimination of
//exceptions based on type. This is a primitive "match" construct. It
//turns out, this is enough to encode sum types.

//This program uses the above idea to implement an interpreter for the
//language of additive expressions using exception handling for case
//discrimination.

//Unfortunately, C++ exceptions on their own are not quite enough, you
//still need to involve RTTI.

#include <iostream>
#include <cassert>
#include <exception>
#include <memory>

struct expr {
  virtual ~expr() {}

  virtual void throw_ () const = 0;
};

using expr_ptr = std::shared_ptr<expr const>;

struct int_ : expr { 
  int val; 
  int_ (int val) : val{val}
  {}

  void throw_ () const { throw *this; } 
};

struct add : expr { 
  expr_ptr left; 
  expr_ptr right; 

  template <class U, class V>
  add (U const& left, V const& right) 
    : left {expr_ptr{new U{left}}}
    , right {expr_ptr{new V{right}}}
  {}

  void throw_ () const { throw *this; } 
};

//These next two functions are mutually recursive

int eval_rec ();

int eval (expr const& xpr) {
  try {
    xpr.throw_ ();
  }
  catch (...) {
    return eval_rec ();
  }
}

int eval_rec () {
  assert (std::current_exception());

  try {
    throw;
  }
  catch (int_ const& i) {
    return i.val;
  }
  catch (add const& op) {
    return eval (*op.left) +  eval (*op.right);
  }
}

int main () {

  try{
    // (1 + 2) + 3
    std::cout << eval (add{add{int_{1}, int_{2}}, int_{3}}) << std::endl;
  }
  catch (...){
    std::cerr << "Unhandled exception\n";
  }
    
  return 0;
}
