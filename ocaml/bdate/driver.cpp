#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

#include <stdlib.h>
#include <stdio.h>

void  flexdll_dump_exports(void* u){(void)u;}
void* flexdll_dlopen(char const* file,int mode){(void)file;(void)mode;return (void*)0;}
void  flexdll_dlclose(void* u){(void)u;}
void* flexdll_dlsym(void* u, const char * n){(void)u;(void)n;return (void*)0;}
char* flexdll_dlerror()
{
  static char* flexdll_error_buffer = "flexdll is not availble";
  return flexdll_error_buffer;
}

int main (int argc, char** argv)
{
  char const* ret;
  value res, * closure;
  char* double_array[2];

  struct caml__roots_block blk, * frame = caml_local_roots;
  blk.next = caml_local_roots;
  blk.nitems = 1;
  blk.ntables = 1;
  blk.tables [0] = &res;
  caml_local_roots = &blk;

  double_array[0] = (char*)malloc(1);
  double_array[0][0] = 0;
  double_array[1] = NULL;
  caml_startup (double_array);
  caml_main (argv);
  closure = caml_named_value ("version");
  if (closure == NULL)
    {
      printf ("\"version\" unavailable!\n");
      goto cleanup;
    }
  else
    {
      res = caml_callback (*closure, Val_unit);/*apply version*/
    }

  printf ("Version: %s\n", String_val (res));

 cleanup:
  free (double_array[0]);
  caml_local_roots = frame;

  return 0;
}
