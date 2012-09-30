#include <cpp/poly_var/poly_var.h>
#include <cpp/poly_var/poly_var_serialization.h>

#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/scoped_ptr.hpp>

#include <iostream>
#include <sstream>

namespace
{
  void test_with_text_archive()
  {
    std::ostringstream os; //Buffer.
    //A poly_var embedding an array of poly_vars.
    poly_var from(1), until(10);
    boost::scoped_ptr<poly_var> l(pv_integers(&from, &until));
    {//Save.
      boost::archive::text_oarchive oa(os);
      oa << *l;
    }
    //std::cout << os.str() << std::endl;
    poly_var m;//Load.
    {
      std::istringstream is(os.str());
      boost::archive::text_iarchive ia(is);
      ia >> m;
    }
    BOOST_TEST_EQ(m.type, l->type);
    BOOST_TEST_EQ(l->type, pv_type_multi);
    BOOST_TEST_EQ(m.rows(), 10);
    BOOST_TEST_EQ(m.columns(), 1);
    for(std::size_t i = 0; i < m.rows(); ++i)
      BOOST_TEST_EQ(m(i, 0).as<int>(), i+1);
  }

  void test_with_binary_archive()
  {
    std::ostringstream os(std::ios_base::binary|std::ios_base::out); //Buffer.
    //A poly_var embedding an array of poly_vars.
    poly_var from(1), until(10);
    boost::scoped_ptr<poly_var> l(pv_integers(&from, &until));
    {//Save.
      boost::archive::binary_oarchive oa(os);
      oa << *l;
    }
    //std::cout << os.str() << std::endl;
    poly_var m;//Load.
    {
      std::istringstream is(os.str(), std::ios_base::binary|std::ios_base::in);
      boost::archive::binary_iarchive ia(is);
      ia >> m;
    }
    BOOST_TEST_EQ(m.type, l->type);
    BOOST_TEST_EQ(l->type, pv_type_multi);
    BOOST_TEST_EQ(m.rows(), 10);
    BOOST_TEST_EQ(m.columns(), 1);
    for(std::size_t i = 0; i < m.rows(); ++i)
      BOOST_TEST_EQ(m(i, 0).as<int>(), i+1);
  }

  void test_with_xml_archive()
  {
    std::ostringstream os; //Buffer.
    //A poly_var embedding an array of poly_vars.
    poly_var from(1), until(10);
    boost::scoped_ptr<poly_var> l(pv_integers(&from, &until));
    {//Save.
      boost::archive::xml_oarchive oa(os);
      oa << boost::serialization::make_nvp("top", *l);
    }
    std::cout << os.str() << std::endl;
    poly_var m;//Load.
    {
      std::istringstream is(os.str());
      boost::archive::xml_iarchive ia(is);
      ia >> boost::serialization::make_nvp("top", m);
    }
    BOOST_TEST_EQ(m.type, l->type);
    BOOST_TEST_EQ(l->type, pv_type_multi);
    BOOST_TEST_EQ(m.rows(), 10);
    BOOST_TEST_EQ(m.columns(), 1);
    for(std::size_t i = 0; i < m.rows(); ++i)
      BOOST_TEST_EQ(m(i, 0).as<int>(), i+1);
  }

}//namespace<anonymous>

int main()
{
  try
  {
    test_with_text_archive();
    test_with_binary_archive();
    test_with_xml_archive();
  }
  catch(std::exception const& e)
  {
    std::cerr << e.what() << '\n';
  }
  catch(...)
  {
    std::cerr << "Unexepected exception\n";
  }

  return boost::report_errors();
}
