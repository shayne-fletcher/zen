#include <type_traits>
#include <iostream>

//union optional[T]
//  | Nothing
//  | Some of T
//  ;

template<class T> struct _SomeOf; //Fwd. decl.
template<class T> _SomeOf<T> SomeOf(T const& x);//Fwd. decl.

struct _Nothing
{
  template<class F>
  _Nothing operator()(F f)
  {
    return _Nothing();
  }

  template<class A, class F>
  _Nothing operator()(A default_, F f)
  {
    return _Nothing();
  }
};
_Nothing Nothing() { return _Nothing(); }
std::ostream& operator << (std::ostream& os, _Nothing) { os << "Nothing" ; return os; }

template<class T>
struct _SomeOf
{
  T x;

  explicit _SomeOf(T x) : x(x) {}

  template<class F>
  auto operator()(F f) -> decltype(f(x))
  {
    return f(x);
  }

  template<class A, class F>
  T operator()(A default_, F f)
  {
    return f(x);
  }

};
template<class T> _SomeOf<T> SomeOf(T const& x)  { return _SomeOf<T>(x); }

int main()
{
  auto a = SomeOf(2);
  auto b = Nothing();

  auto f = [](int i){ return i*i; };

  int const default_=-1;
  auto fa = a(f);
  auto fb = b(f);
  
  try
  {
    std::cout << "fa=" << fa << std::endl;
    std::cout << "fb=" << fb << std::endl;
  }
  catch(std::runtime_error const& e)
  {
    std::cerr << e.what() << '\n';
  }

  return 0;
}
