#include <iostream>
#include <functional>

//Expr = X | Const A | BinOp Expr Expr

struct _X
{
  template <class T>
  T operator()(T x) { return x; }
};

_X X() { return _X(); }

auto const x = X();

template<class T>
struct _Const
{
  T a;
  _Const(T a) : a(a) {}
  T operator()(T x) { return a; }
};

template<class T> 
_Const<T> Const(T a) 
{ return _Const<T>(a); }

template<class Op
 , class L, class R //requires operator()
>
struct _BinOp
{
  Op op;
  L l;
  R r;
  _BinOp(Op op, L l, R r) : op(op), l(l), r(r){}
  template<class T> T operator()(T x) { return op(l(x), r(x));}
};

template<class Op, class E1, class E2> 
_BinOp<Op,  E1, E2>
BinOp(Op op, E1 l, E2 r){ return _BinOp<Op, E1, E2>(op, l, r); }

template<class T
	 , class Expr //requires operator()
> 
T eval(T a, Expr e) { return e(a); }

template<class E1, class E2>
_BinOp<std::plus<int>, E1, E2> operator + (E1 e1, E2 e2) 
{
  return BinOp(std::plus<int>(), e1, e2);
}

template<class E1, class E2>
_BinOp<std::minus<int>, E1, E2> operator - (E1 e1, E2 e2) 
{
  return BinOp(std::minus<int>(), e1, e2);
}

template<class E1, class E2>
_BinOp<std::multiplies<int>, E1, E2> operator * (E1 e1, E2 e2) 
{
  return BinOp(std::multiplies<int>(), e1, e2);
}

template<class E1, class E2>
_BinOp<std::divides<int>, E1, E2> operator / (E1 e1, E2 e2) 
{
  return BinOp(std::divides<int>(), e1, e2);
}

int main()
{
  auto Plus=  [](int x, int y) -> int { return x+y; };
  auto Minus= [](int x, int y) -> int { return x-y; };
  auto Times= [](int x, int y) -> int { return x*y; };
  auto Divide=[](int x, int y) -> int { return x/y; };

  auto const two = Const(2);

  std::cout << eval(3, x) << std::endl;
  std::cout << eval(3, two) << std::endl;
  std::cout << eval(3, BinOp(Plus,  x, two)) << std::endl;

  auto expr = x + two;
  std::cout << eval(3, expr) << std::endl;
  std::cout << eval(3, x*x) << std::endl;

  return 0;
}
