//"c:/program files (x86)/Microsoft Visual Studio 14.0/vc/vcvarsall.bat" x64
//cl /Fecat.exe /Zi /MDd /EHsc /I d:/project/shayne-fletcher.github/pretty-good-sum.git/src cat.cpp

//A small family of some sort of trivial "category theoretic" types
//and functions

#include <pgs/pgs.hpp>

#include <utility>
#include <iostream>
#include <string>

//--

//Products

template <class A, class B>
using product = std::pair<A, B>;

//`fst (x, y)` is the projection `x`
template <class A, class B>
inline auto fst (product<A, B> const& p) {
  return p.first;
}

//`snd (x, y)` is the projection `y`
template <class A, class B>
inline auto snd (product<A, B> const& p) {
  return p.second;
}

//`mk_product (a, b) computes the product of `a` and `b`
template <class A, class B>
inline product<A, B> mk_product (A&& a, B&& b) {
  return  std::make_pair (std::forward<A> (a), std::forward<B> (b));
}

//A printing utility for products
template <class A, class B>
inline std::ostream& 
operator << (std::ostream& os, product<A, B> const& p) {
  return os << "(" << fst (p) << ", " << snd (p) << ")";
}

//`dup a` computes the product `(a, a)`
template <class A>
inline product<A, A> dup (A const& x) { return mk_product (x, x); }

//The product of morphisms <`f`, `g`> (see
//https://en.wikipedia.org/wiki/Product_(category_theory))
auto prod = [=](auto f, auto g) {
  return [=](auto const& x) { return mk_product (f (x), g (x)); };
};
//For objects, X, Y and Z, `f` : Z -> X, `g` : Z -> Y, <`f`, `g`> is
//the mediating arrow from Z to X * Y.

//`twist (x, y)` is the product `(y, x)`
template <class A, class B>
inline product<B, A> twist (product<A, B> const& p) {
  return mk_product (snd (p), fst (p));
}

//`ravel f g (x, y)` is the product `(f x, g y)` (credit to Max
//Skaller on the name)
auto ravel = [=](auto f) {
  return [=](auto g) {
    return [=](auto const& x) { 
      return mk_product (f (fst (x)), g (snd (x))); 
    };
  };
};

// -- 

//Sums

template <class A>
struct Left { 
  A a; 
  template <class U> explicit Left (U&& u) : a {std::forward<U> (u)} {}
  A const& value () const { return a; }
};

template <class B>
struct Right { 
  B b; 
  template <class U> explicit Right (U&& u) : b {std::forward<U> (u)} {}
  B const& value () const { return b; }
};

template <class A, class B>
using sum = pgs::sum_type<Left<A>, Right<B>>;
template <class> struct sum_fst_type;
template <class A, class B>
  struct sum_fst_type<sum<A, B>> { typedef A type; };
template <class S> struct sum_snd_type;
template <class A, class B>
  struct sum_snd_type<sum<A, B>> { typedef B type; };

//The coproduct of morphisms [`f, `g`] (see
//https://en.wikipedia.org/wiki/Coproduct)
template <class S>
auto co_product = [=](auto f) {
  return [=](auto g) {
    return [=](S const& s) {
      using A = typename sum_fst_type<S>::type;
      using B = typename sum_snd_type<S>::type;
      using lres_t = decltype (f (std::declval<A>()));
      using rres_t = decltype (g (std::declval<B>()));
      static_assert (
        std::is_same<lres_t, rres_t>::value
       , "co_product : result types differ");
      using res_t = lres_t;
      return s.match<lres_t>(
        [=](Left<A> const& l) { return f (l.value ()); },
        [=](Right<B> const& r) { return g (r.value ()); }
       );
    };
  };
};
//For objects X, Y and Z, `f` : X -> Z, `g` : Y -> Z, the mediating
//arrow from X + Y.

//--

//Currying

//`curry f` where `f` : X * Y -> Z` computes \x. \y. f (x, y),
//that is, a multi-argument function
auto curry = [=](auto f) {
  return [=](auto const& x) {
    return [=](auto const& y) {
      return f (mk_product(x, y));
    };
  };
};

//`uncurry f` where `f` : X -> Y -> Z produces a new function `\(x,
//y). f x y`, that is a single argument function from X * Y -> Z
auto uncurry = [](auto f) {
  return [=](auto const& p) { return f (fst (p)) (snd (p)); };
};

//--

//Reverse application (pipeline)

//Pipeline operator (`x / f` means `f (x)`)
template <class X, class F>
inline auto operator / (X x, F f) {
  return f (x);
}

//Function composition operator (right associative) 
template <class F, class G>
inline auto operator |= (F f, G g) {
  return [=](auto x) { return g (f (x)); };
}

//"Command operator" (right associative)
template <class F, class X>
inline auto operator %= (F f, X const& x) {
  return f (x);
}

//--

int main () {

  auto succ=[](int i) { return i + 1; };
  auto pred=[](int i) { return i - 1; };
  auto twice=[](int i) { return 2 * i; };
  auto square=[](int i) { return i * i; };
  auto add=[](product<int, int> const& s) { return (fst (s) + snd (s)); };

  //--

  //Products

  //`dup`
  auto p = dup (1);
  std::cout << p << std::endl; //Prints '(1, 1)'

  //`prod`
  p = prod (succ, pred) (4);
  std::cout << p << std::endl; //Prints '(5, 3)'

  //`twist`
  p = twist (p);
  std::cout << p << std::endl; //Prints '(3, 5)'
  
  //`ravel`
  p = ravel (succ) (pred) (p);
  std::cout << p << std::endl; //Prints '(4, 4)'

  //--

  //Sums

  sum<int, float> l{pgs::constructor<Left<int>>{}, 1};
  sum<int, float> r{pgs::constructor<Right<float>>{}, 1.0f};
  std::cout << 
    co_product<sum<int, float>> 
      ([=](int i) { return std::to_string(i); })
      ([=](float f) { return std::to_string(f); })
      (l)
    << std::endl;
  ;//Prints '1'
  std::cout << 
    co_product<sum<int, float>> 
      ([=](int i) { return std::to_string(i); })
      ([=](float f) { return std::to_string(f); })
      (r)
    << std::endl;
  ;//Prints '1.000000'

  //--

  //Currying

  //`curry`
  std::cout << curry (add) (1) (2) << std::endl;//Prints '3'

  //`uncurry`
  std::cout << uncurry (curry (add))(mk_product (1, 2)) << std::endl;
                                                 //Prints '3'

  //--

  //Functional operators

  //Reverse application (pipelines)
  std::cout <<  3 / twice / succ << std::endl; //Prints '7'

  //Function composition
  std::cout << (succ |= twice |= square) (3) << std::endl; //Prints '64'
  
  //Command operator (right associative function application)
  std::cout << (succ %= 2 + 3) << std::endl; //Prints '6'

  return 0;
}
