//"c:/users/fletch/desktop/setenv.cmd"
//cl /EHsc /Feconcat.exe /I c:/project/boost_1_55_0 concat.cpp 

#include <boost/mpl/fold.hpp>
#include <boost/mpl/reverse.hpp>
#include <boost/mpl/push_front.hpp>
#include <boost/mpl/push_back.hpp>
#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/vector.hpp>

using namespace boost::mpl::placeholders;

struct cat //'lambda expression' (metafunction class)
{
  template <class L1, class L2>
  struct apply
  {
    typedef typename boost::mpl::fold<
      typename boost::mpl::reverse<L1>::type/*s*/
    , L2 /*state*/
    , boost::mpl::push_front<_1, _2>/*op*/ >::type type;
  };
};

struct concat //'lambda expression' (metafunction class)
{
  template <class L>
  struct apply
  {
    typedef typename boost::mpl::fold <L, boost::mpl::vector<>, cat>::type type;
  };
};

//--
//test

#include <boost/mpl/equal.hpp>
#include <boost/mpl/int.hpp>

int main ()
{
  using namespace boost::mpl;

  typedef 
    vector <
       vector<int_<1>, int_<2> >
     , vector<int_<3>, int_<4> > 
     , vector<int_<5> > 
     , vector<int_<6>, int_<7> > 
    > ls;

  BOOST_MPL_ASSERT ((
    equal<
      apply<concat, ls>::type
    , vector<int_<1>, int_<2>, int_<3>, int_<4>, int_<5>, int_<6>, int_<7> >
    > ));


  return 0;
}
