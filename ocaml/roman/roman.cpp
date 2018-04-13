//g++ -std=c++11 -o roman roman.cpp

#include <stdexcept>
#include <iostream>
#include <iterator>
#include <string>

int dec_of_sym (char c) {
  switch (c) {
  case 'I' : return 1;
  case 'V' : return 5;
  case 'X' : return 10;
  case 'L' : return 50;
  case 'C' : return 100;
  case 'D' : return 500;
  case 'M' : return 1000;
  default : throw std::runtime_error ("dec_of_sym : unrecognized symbol");
  }
}

template <class ItT>
int dec_of_rom_rec (int acc, ItT begin, ItT end) {
  if (begin == end) return acc;

  if (std::next (begin) == end)
    return dec_of_rom_rec (acc + dec_of_sym (*begin), ++begin, end);

  int x = dec_of_sym (*begin);
  int y = dec_of_sym (*(std::next (begin)));
  if (x < y) {
    return dec_of_rom_rec (acc + y - x, std::next(begin, 2), end);
  }
    
  return dec_of_rom_rec (acc + x, std::next(begin, 1), end);
}

template <class ItT>
int dec_of_rom (ItT begin, ItT end) {
  return dec_of_rom_rec (0, begin, end);
}

int main (int argc, char const* argv[]) {
  if (argc != 2) {
    std::cerr << "usage : " << argv[0] << " STRING" << std::endl;

    return 1;
  }

  try
  {
    std::string s{argv[1]};
    std::cout << dec_of_rom (s.cbegin (), s.cend()) << std::endl;
  }
  catch (std::runtime_error const& e) {
    std::cerr << e.what () << std::endl;
  }
  catch (...) {
    std::cerr << "unhandled exception" << std::endl;
  }

  return 0;
}
