#include <string.h>

#include <caml/mlvalues.h>
#include <caml/callback.h>

int fib(int n)
{
  static value* fib_closure=(value*)0L;
  if(!fib_closure)
    {
      fib_closure = caml_named_value("fib");

    }

  return Int_val(caml_callback(*fib_closure, Val_int(n)));
}

char* format_result(int n)
{
  int len;
  char*s,*c;
  static value* format_result_closure=(value*)0L;
  if(!format_result_closure)
    format_result_closure = caml_named_value("format_result");

  s=String_val(caml_callback(*format_result_closure, Val_int(n)));

  //Copy contents of s to the heap so that it remains valid after
  //garbage collection.

  len=strlen(s);
  c = (char*)malloc(len+1);
  strcpy(c, s);
  
  return c;
}

