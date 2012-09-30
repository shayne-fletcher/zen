#include <stdio.h>
#include <stdlib.h>

void caml_startup(char**);

int fib(int n);
char* format_result(int n);

int main(int argc, char** argv)
{
  int result;
  char* s;

  caml_startup(argv);

  result=fib(10);

  printf("fib(10) = %s\n", s=format_result(result));

  free(s);

  return 0;
}
