#include "list.hpp"

#include <boost/assign/std/vector.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/spirit/home/phoenix/bind/bind_function.hpp>
#include <boost/foreach.hpp>

int main()
{
  try
  {
    using namespace boost::assign;
    using boost::phoenix::arg_names::arg1;
    using boost::phoenix::arg_names::arg2;

    std::vector<int> data;
    data +=
        6
      , 5
      , 4
      , 3
      , 2
      , 1
      ;

    std::cout << "Orig:\n";
    list::show(data, std::cout);

    //even :: [a] -> [a]
    //even xs = [x | x <- xs, x % 2 == 0]

    std::vector<int> even;
    list::filter(data, std::back_inserter(even), arg1 % 2 == 0);
    std::cout << "Evens:\n";
    list::show(even, std::cout);

    //odd :: [a] -> [a]
    //odd xs = [x | x <- xs,  x % 2 /= 0]

    std::vector<int> odd;
    list::filter(data, std::back_inserter(odd), arg1 % 2 != 0);
    std::cout << "Odds:\n";
    list::show(odd, std::cout);

    //doubleEven :: [a] -> [a]
    //doubleEven xs = [2*x | x <- xs, x % 2 == 0]

    std::vector<int> double_even;
    list::comprehend(
      data, std::back_inserter(double_even), 2*arg1, arg1 % 2 == 0);
    std::cout << "Double evens:\n";
    list::show(double_even, std::cout);

    //squareEven :: [a] -> [a]
    //squareEven xs = [x*x | x <- xs, x % 2 == 0]

    std::vector<int> square_even;
    list::comprehend(
      data, std::back_inserter(square_even), arg1*arg1, arg1 % 2 == 0);
    std::cout << "Squared evens:\n";
    list::show(square_even, std::cout);

    //reversed :: [a] -> [a]

    std::vector<int> reversed;
    list::reverse(data, std::back_inserter(reversed));
    std::cout << "Reversed:\n";
    list::show(reversed, std::cout);

    std::vector<int> quick_sort;
    list::quick_sort(data, std::back_inserter(quick_sort));
    std::cout << "Quick sort:\n";
    list::show(quick_sort, std::cout);

    // take :: [a] -> Int -> [a]
    
    std::vector<int> take;
    list::take(data, std::back_inserter(take), 3);
    std::cout << "Take (3):\n";
    list::show(take, std::cout);

    // take_while :: (a -> bool) -> [a] ->[a]
  
    std::vector<int> take_while;
    list::take_while(data, std::back_inserter(take_while), arg1 > 3);
    std::cout << "Take while:\n";
    list::show(take_while, std::cout);

    // drop :: Int -> [a] -> [a]

    std::vector<int> drop;
    list::drop(data, std::back_inserter(drop), 3);
    std::cout << "Drop (3):\n";
    list::show(drop, std::cout);

    // drop_while :: [a] -> (a -> Bool) -> []

    std::string drop_while;
    list::drop_while(
       std::string("abc def")
       , std::back_inserter(drop_while)
       , static_cast<int(*)(int)>(std::islower));
    std::cout << "Drop while:\n";
    std::cout << drop_while << '\n' << std::endl;

    // init :: [a] -> [a]

    std::vector<int> init;
    list::init(data, std::back_inserter(init));
    std::cout << "Init:\n";
    list::show(init, std::cout);
    
    // concat :: [[a]] -> [a]
   
    std::vector<int> concat;
    list::concat(list_of
        (boost::make_iterator_range(data.begin()+3, data.end()))
        (boost::make_iterator_range(data.begin(), data.begin() + 3))
      , std::back_inserter(concat));
    std::cout << "Concat:\n";
    list::show(concat, std::cout);

    // replicate :: a -> Int -> [a]
    
    std::vector<int> replicate;
    list::replicate(1, 5, std::back_inserter(replicate));
    std::cout << "Replicate:\n";
    list::show(replicate, std::cout);

    // select :: [a] -> int -> a
  
    int val_at_idx_3 = list::select(data, 3);
    std::cout << "Select:\n" << val_at_idx_3 << '\n' << std::endl;
    
    // merge :: [a] -> [a] -> [a]

    std::vector<int> merge;
    list::merge(
        boost::assign::list_of(2)(5)(6)
      , boost::assign::list_of(1)(3)(4)
      , std::back_inserter(merge));
    std::cout << "Merge:\n";
    list::show(merge, std::cout);

    // merge_sort :: [a] -> [a]
 
    std::vector<int> merge_sort;
    list::merge_sort(data, std::back_inserter(merge_sort));
    std::cout << "Merge sort:\n";
    list::show(merge_sort, std::cout);
    
    // elem :: [a] -> a -> bool

    std::cout << "Elem:\n";
    std::cout << list::elem(data, 0) << std::endl;
    std::cout << list::elem(data, 6) << '\n' << std::endl;

    // sum :: [a] -> a
    
    std::cout << "Sum:\n";
    std::cout << list::sum(data) << '\n' << std::endl;

    // product :: [a] -> a

    std::cout << "Product:\n";
    std::cout << list::product(data) << '\n' << std::endl;

    // sorted :: [a] -> Bool

    std::cout << "Sorted:\n";
    std::cout << list::sorted(data) << std::endl;
    std::cout << list::sorted(quick_sort) << '\n' << std::endl;

    // head :: [a] -> a

    std::cout << "Head:\n" << list::head(data) << '\n' << std::endl;;

    // tail :: [a] -> [a]

    std::vector<int> tail;
    std::cout << "Tail:\n";
    list::tail(data, std::back_inserter(tail));
    list::show(tail, std::cout);

    // last :: [a] -> a

    std::cout << "Last:\n" << list::last(data) << '\n' << std::endl;;

    // pairs :: [a] -> [(a, a)]
  
    typedef boost::tuple<int, int> pair_t;
    std::vector<boost::tuple<int, int> > pairs;
    std::cout << "Pairs:\n";
    list::pairs(quick_sort, std::back_inserter(pairs));
    BOOST_FOREACH(pair_t p, pairs)
      std::cout << '('
                << boost::get<0>(p)
                << ", "
                << boost::get<1>(p)
                << ")\n";
    std::cout << std::endl;

    // cartesian_product :: [a] -> [a] -> [(a, a)]

    std::vector<boost::tuple<int, int> > cartesian_product;
    list::cartesian_product(
         boost::assign::list_of(1)(2)(3)
       , boost::assign::list_of(4)(5)
       , std::back_inserter(cartesian_product)
       );
    std::cout << "Cartesian product:\n";
    BOOST_FOREACH(pair_t p, cartesian_product)
      std::cout << '('
                << boost::get<0>(p)
                << ", "
                << boost::get<1>(p)
                << ")\n";
    std::cout << std::endl;
    
    // zip :: [a] -> [a] -> [(a, a)]

    std::vector<boost::tuple<int, int> > zip;
    list::zip(
        boost::assign::list_of(1)(3)(5)
      , boost::assign::list_of(2)(4)(6)
      , std::back_inserter(zip));
    std::cout << "Zip:\n";
    BOOST_FOREACH(pair_t p, zip)
      std::cout << '('
                << boost::get<0>(p)
                << ", "
                << boost::get<1>(p)
                << ")\n"
                << std::endl;
    
    // zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
  
    std::vector<int> zip_with;
    list::zip_with(
          2*arg1+arg2
        , boost::make_iterator_range(
              boost::make_counting_iterator(1)
            , boost::make_counting_iterator(5))
        , boost::make_iterator_range(
              boost::make_counting_iterator(5)
            , boost::make_counting_iterator(9))
        , std::back_inserter(zip_with)
        );
    std::cout << "Zip with:\n";
    list::show(zip_with, std::cout);

    // unzip :: [(a, a)] -> ([a], [a])

    std::vector<int> unzip_left, unzip_right;
    list::unzip(
       boost::assign::
         list_of(boost::make_tuple(1, 2))(boost::make_tuple(3, 4))
     , std::back_inserter(unzip_left), std::back_inserter(unzip_right)
     );
    std::cout << "Unzip\n";
    list::show(unzip_left, std::cout);
    list::show(unzip_right, std::cout);

    // minimum : [a] -> a

    std::cout << "Minimum:\n"
              << list::minimum(boost::assign::list_of(4)(6)(1)(5))
              << '\n' << std::endl;

    // maximum : [a] -> a

    std::cout << "Maximum:\n"
              << list::maximum(boost::assign::list_of(4)(6)(1)(5))
              << '\n' << std::endl;

    // scanl :: (a -> [b] -> a) -> a -> [b] -> [a]

    std::cout << "Scan l (1):\n";
    std::vector<double> scanl;
    list::scanl(
        std::divides<double>()
      , 64.
      , boost::assign::list_of(4.)(2.)(4.)
      , std::back_inserter(scanl));
    list::show(scanl, std::cout);
    scanl.clear();
    std::cout << "Scan l (2):\n";
    list::scanl(
        std::divides<double>()
      , 3.
      , std::vector<double>()
      , std::back_inserter(scanl));
    list::show(scanl, std::cout);

    // scanl1 :: (a -> a -> a) -> [a] -> [a]

    std::cout << "Scan l1 (1):\n";
    std::vector<int> scanl1;
    list::scanl1(
       std::plus<int>()
     , boost::assign::list_of(1)(2)(3)(4)
     , std::back_inserter(scanl1));
    list::show(scanl1, std::cout);

    // span :: (a -> bool) -> [a] -> ([a], [a])

    std::cout << "Span:\n";
    std::string left, right;
    list::span(
        std::string("123abc456")
      , std::back_inserter(left)
      , std::back_inserter(right)
      , static_cast<int(*)(int)>(std::isdigit));
    std::cout << "(left) " << left << '\n' << std::endl;
    std::cout << "(right) " << right << '\n' << std::endl;
    left.clear(), right.clear();

    // break :: (a -> bool) -> [a] -> ([a], [a])
    
    std::cout << "Break_:\n";
    list::break_(
         std::string("abc456")
       , std::back_inserter(left)
       , std::back_inserter(right)
       , std::ptr_fun(static_cast<int(*)(int)>(std::isdigit)));
    std::cout << "(left) " << left << '\n' << std::endl;
    std::cout << "(right) " << right << '\n' << std::endl;

    // length :: [a] -> int

    std::cout << "length:\n"
              << list::length(boost::assign::list_of(1)(2)(3))
              << '\n'
              << std::endl;

    // lines :: String -> [String]

    std::string text="It was the best of times.\nIt was the worst of times.\n";
    std::vector<std::string> lines;
    list::lines(text, std::back_inserter(lines));
    std::cout << "Lines:\n";
    BOOST_FOREACH(std::string const& line, lines)
      std::cout << line << '\n';
    std::cout << std::endl;

    // lines :: String -> [String]

    std::vector<std::string> words;
    list::words(text, std::back_inserter(words));
    std::cout << "Words:\n";
    BOOST_FOREACH(std::string const& line, words)
      std::cout << line << '\n';
    std::cout << std::endl;

    // append :: [a] -> [a] -> [a]

    std::vector<int> append;
    list::append(
        boost::assign::list_of(1)(2)
      , boost::assign::list_of(3)(4)
      , std::back_inserter(append));
    std::cout << "Append:\n";
    list::show(append, std::cout);

    // subrange :: [a] -> Int -> [[a]]

    std::vector<int> tmp;
    std::vector<std::vector<int> > subrange;
    std::cout << "Subrange:\n";
    list::subrange(
       tmp = boost::assign::list_of(1)(2)(3)
     , std::back_inserter(subrange)
     , 2);
    BOOST_FOREACH(std::vector<int> const& seq, subrange)
      list::show(seq, std::cout);

    // subsets :: [a] -> [[a]]

    std::vector<std::vector<int> > subsets;
    std::cout << "Subsets:\n";
    list::subsets(
       tmp = boost::assign::list_of(1)(2)(3)
     , std::back_inserter(subsets));
    BOOST_FOREACH(std::vector<int> const& seq, subsets)
      list::show(seq, std::cout);

    // fold :: (a -> b -> a) -> [b] -> [a]

    std::cout << "Foldl:\n";
    std::cout <<
                 list::foldl(
                 boost::assign::list_of(4)(2)(4)
                 , 64.
                 , std::divides<double>()
                 )
              << '\n'
              << std::endl;

    // foldl :: (a -> b -> a) -> [b] -> [a]

    std::cout << "Foldl:\n";
    std::cout <<
                 list::foldl(
                   boost::assign::list_of(1)(2)(3)(4)
                 , 0
                 , boost::phoenix::bind(&(std::max<int>), arg1, arg2)
                 )
              << '\n'
              << std::endl;

    // foldr :: (a -> b -> b) -> b -> [a] -> b

    std::cout << "Foldr:\n";
    std::cout <<
                 list::foldr(
                   boost::assign::list_of(1)(2)(3)(4)
                 , 5
                 , std::plus<int>()
                 )
              << '\n'
              << std::endl;

    // any :: (a -> Bool) -> [a] -> Bool

    std::cout << "Any:\n"
              << list::any(boost::assign::list_of(0)(2), arg1 % 2 !=0 ) << '\n';
    std::cout << "Any:\n"
              << list::any(boost::assign::list_of(0)(2), arg1 % 2 == 0) << '\n'
              << std::endl;

    // all :: (a -> Bool) -> [a] -> Bool

    std::cout << "All:\n"
              << list::all(boost::assign::list_of(0)(2), arg1 < 3) << '\n';
    std::cout << "All:\n"
              << list::all(boost::assign::list_of(0)(2), arg1 >= 1) << '\n'
              << std::endl;

  }
  catch(std::exception const& e)
  {
    std::cerr << e.what() << '\n';
  }
  catch(...)
  {
    std::cerr << "unexpected exception\n";
  }

  return 0;
}
