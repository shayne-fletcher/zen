#include <iostream>

//An interpreter for the language of additive expressions using
//exception handling for case discrimination. Sadly, C++ exceptions on
//their own are not enough, you still have to involve RTTI.

struct expr {
  virtual ~expr() {}
};

using expr_ptr = expr*;

struct int_ : expr { 
  int val; 
  int_ (int val) : val{val}
  {}
};

struct add : expr { 
  expr_ptr left; 
  expr_ptr right; 

  add (expr_ptr left, expr_ptr right) 
    : left {left}, right {right}
  {}
};

int eval_rec () {
  try {
    throw;
  }
  catch (int_ const& i) {
    return i.val;
  }
  catch (add const& op) {
    int left, right;
    try{
      //Wouldn't it be nice if this block could be replaced with just,
      //`throw *op.left`?
      if (add* p = dynamic_cast<add*>(op.left)) throw *p;
      if (int_* p = dynamic_cast<int_*>(op.left)) throw *p;
    }
    catch (...) {
      left = eval_rec ();
    }
    try{
      if (add* p = dynamic_cast<add*>(op.right)) throw *p;
      if (int_* p = dynamic_cast<int_*>(op.right)) throw *p;
    }
    catch (...) {
      right = eval_rec ();
    }
    return left + right;
  }
}

template <class T>
int eval (T const& xpr) {
  try {
    throw xpr;
  }
  catch (...) {
    return eval_rec ();
  }
}

int main () {

  try{
    // (1 + 2) + 3
    int_ one{1}, two{2};
    add inner{&one, &two};
    int_ three{3};
    add outer(&inner, &three);
    std::cout << eval (outer) << std::endl; // 6
  }
  catch (...){
    std::cerr << "Unhandled exception\n";
  }
    
  return 0;
}
