#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

#include <assert.h>
#include <string.h>
#include <stdio.h>

#if defined (_MSC_VER)
#  pragma warning(disable:4996)
#endif//defined(_MSC_VER)

void c_echo(char const* p, char** res)
{
  CAMLparam0();

  char* result;
  value* closure=(value*)0L;

  CAMLlocal1(in);
  in = caml_copy_string(p);
  closure = caml_named_value("caml_echo");
  assert(closure);
  result=String_val(caml_callback (*closure, in));
  assert(result);

  *res = strdup(result);

  CAMLreturn0;
}
