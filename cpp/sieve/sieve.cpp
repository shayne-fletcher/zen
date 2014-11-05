//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl /Fesieve.exe /EHsc /Id:/boost_1_55_0 sieve.cpp

#include <boost/mpl/if.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/push_back.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/for_each.hpp>
#include <boost/mpl/fold.hpp>

#include <iostream>

using namespace boost::mpl;
using namespace boost::mpl::placeholders;

template <class P, class L>
struct filter
{
  template <class AccT, class T>
  struct f {
    typedef typename 
    if_<typename apply<P, T>::type
        , typename push_back<AccT, T>::type, AccT>::type type;
  };

  typedef typename fold <L, vector<>, f<_1, _2> >::type type;
};

template <class I>
struct is_even {
 typedef bool_<I::value % 2 == 0> type;
 enum {value = type::value};
};

typedef vector<int_<1>, int_<2>, int_<3>, int_<4> > ls;
typedef filter<is_even<_1>, ls>::type result;

struct print_class_name {
    template <typename T>
    void operator()( T t ) const {
       std::cout << typeid(t).name() << " ";
    }
};

int main ()
{
  boost::mpl::for_each<result>(print_class_name());

  return 0;
}
