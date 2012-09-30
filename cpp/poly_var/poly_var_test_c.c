#include <cpp/poly_var/poly_var.h>

#include <stdio.h>

int main()
{
  size_t i;
  poly_var from, until,*p, *q, *r;

  poly_var_api_initialize();

  /*What compiler?*/

  r = poly_var_api_call_by_name("compiler", 0);
  printf("Compiler : %s\n", r->val.str);
  poly_var_clear(r);
  free(r);

  /*Directly invoke integers().*/

  poly_var_init(&from);
  from.val.w = 1;
  from.type = pv_type_int;

  poly_var_init(&until);
  until.val.w = 10;
  until.type = pv_type_int;

  p = pv_integers(&from, &until);
  for(i = 0; i < p->val.array.rows; ++i)
  {
    printf("%d ", p->val.array.data[i].val.w);
  }
  printf("\n");
  poly_var_clear(p);
  free(p);

  /*Invoke integers() indirectly by name.*/

  q = poly_var_api_call_by_name("integers", 2, &from, &until);
  for(i = 0; i < q->val.array.rows; ++i)
  {
    printf("%d ", q->val.array.data[i].val.w);
  }
  printf("\n");
  poly_var_clear(q);
  free(q);

  poly_var_api_uninitialize();

  return 0;
}

