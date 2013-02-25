#include <string>
#include <iostream>

extern "C" void caml_startup (char**);
extern "C" void c_echo (char const* p, char**);

static inline 
std::string mk_string(int i)
{
  int N=i*1024*1024;

  return std::string (N, '*');
}

int main()
{
  char *double_array[2];
  double_array[0] = (char *)malloc(1);
  double_array[0][0] = 0;
  double_array[1] = NULL;
  caml_startup(double_array);

  std::string s= mk_string(1);
  char* p;
  c_echo (s.c_str(), &p);

  free(p);

  return 0;
}
