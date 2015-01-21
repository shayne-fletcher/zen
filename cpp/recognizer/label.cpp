//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl /Felabel.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 label.cpp

#include <utility>
#include <functional>
#include <memory>
#include <iostream>

std::pair<
    std::function<void ()>
  , std::function< int ()> >
make_label_generator ()
{
  std::shared_ptr<int> r (new int (-1)
   , [=](int* p) {delete p ; std::cout << "delete ptr\n"; } );
  
  return std::make_pair ([=] () { *r = -1; }, [=] () { return *r += 1, *r; });
}

int main ()
{
  auto p = make_label_generator ();

  for (int i = 0; i < 10; ++i)
    std::cout << std::get <1> (p) () << "\n";

  std::get <0> (p) ();

  for (int i = 0; i < 10; ++i)
    std::cout << std::get <1> (p) () << "\n";

  return 0;
}
