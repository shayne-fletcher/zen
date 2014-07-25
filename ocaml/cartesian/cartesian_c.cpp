//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl /Fecartesian_c.exe /I d:/boost_1_55_0 cartesian_c.cpp

#include <boost/mpl/pair.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/push_front.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/reverse.hpp>

template <class L, class R>
struct product
{
  struct g {
    template <class AccT, class A>
    struct apply {
      struct f {
        template <class AccT, class X>
        struct apply {
          typedef typename 
             boost::mpl::push_front<
               AccT, boost::mpl::pair <A, X> >::type type;
        };
      };
      typedef typename boost::mpl::fold <R, AccT, f>::type type;
    };
  };
  typedef typename boost::mpl::reverse <
    typename boost::mpl::fold <L, boost::mpl::vector<>, g>::type>::type type;
};

//--
//test

#include <boost/mpl/equal.hpp>
#include <boost/mpl/for_each.hpp>
#include <boost/mpl/int.hpp>

#include <iostream>

using namespace boost::mpl;

typedef vector<int_<1>, int_<2> > A;
typedef vector<int_<3>, int_<4>, int_<5> > B;
typedef product <A, B>::type result;

BOOST_MPL_ASSERT((
  equal<
    result
  , vector<
         pair<int_<1>, int_<3> >
       , pair<int_<1>, int_<4> >
       , pair<int_<1>, int_<5> >
       , pair<int_<2>, int_<3> >
       , pair<int_<2>, int_<4> >
       , pair<int_<2>, int_<5> >
      > 
  > ));

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
