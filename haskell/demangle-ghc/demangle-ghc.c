#define TEST
/*
 * SPDX-License-Identifier: BSD3
 * Copyright 2019 Ben Gamari
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/*
 * See https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/symbol-names
 */

/*
 * Returns number of characters decoded including NULL-terminator or zero if
 * input is malformed fills `out` if not NULL.
 */
static int
__ghc_demangle_sym(const char *in, char *out)
{
  int count = 0;
#define PUSH(c) { if (out) { out[count] = (c); } count++; }

  while (*in) {
    switch (*in) {
      case 'Z':
      {
        in++;
        switch (*in) {
          case '\0': return 0;
          case 'L': PUSH('('); in++; break;
          case 'R': PUSH(')'); in++; break;
          case 'M': PUSH('['); in++; break;
          case 'N': PUSH(']'); in++; break;
          case 'C': PUSH(':'); in++; break;
          case 'Z': PUSH('Z'); in++; break;
          default:
          {
            char *tmp;
            unsigned long arity = strtoul(in, &tmp, 10);
            if (tmp == in)
              return 0;

            in = tmp;
            switch (*in) {
              case '\0': return 0;
              case 'T':
                // boxed tuple
                PUSH('(');
                if (arity > 0) {
                  for (unsigned long i=0; i < arity-1; i++) {
                    PUSH(',');
                  }
                }
                PUSH(')');
                in++;
                break;

              case 'H':
                // unboxed tuple
                PUSH('(');
                PUSH('#');
                if (arity == 1) {
                  PUSH(' ');
                } else {
                  for (unsigned long i=0; i < arity-1; i++) {
                    PUSH(',');
                  }
                }
                PUSH('#');
                PUSH(')');
                in++;
                break;

              default:
                return 0;
            }
            break;
          }
        } /* inner switch */
        break;
      }

      case 'z':
        in++;
        switch (*in) {
          case '\0': return 0;
          case 'a': PUSH('&'); in++; break;
          case 'b': PUSH('|'); in++; break;
          case 'c': PUSH('^'); in++; break;
          case 'd': PUSH('$'); in++; break;
          case 'e': PUSH('='); in++; break;
          case 'g': PUSH('>'); in++; break;
          case 'h': PUSH('#'); in++; break;
          case 'i': PUSH('.'); in++; break;
          case 'l': PUSH('<'); in++; break;
          case 'm': PUSH('-'); in++; break;
          case 'n': PUSH('!'); in++; break;
          case 'p': PUSH('+'); in++; break;
          case 'q': PUSH('\''); in++; break;
          case 'r': PUSH('\\'); in++; break;
          case 's': PUSH('/'); in++; break;
          case 't': PUSH('*'); in++; break;
          case 'u': PUSH('_'); in++; break;
          case 'v': PUSH('%'); in++; break;
          default:
          {
            char *tmp;
            int codepoint = strtoull(in, &tmp, 16);
            if (tmp == in) {
              return 0;
            }
            in = tmp;
            if (*in != 'U') {
              return 0;
            }
            in++;
            if (codepoint > 255)
              codepoint = '~'; // suppress non-ASCII characters
            PUSH(codepoint);
            break;
          }
        }
        break;

      default:
        PUSH(*in);
        in++;
        break;
    }
  } /* while(*c) */
  PUSH('\0');

#undef PUSH
  return count;
}

char *
ghc_demangle_sym(char *sym)
{
  int len = __ghc_demangle_sym(sym, NULL);
  if (len == 0) {
    return sym;
  } else {
    char *out = malloc(len+1);
    __ghc_demangle_sym(sym, out);
    free(sym);
    return out;
  }
}

#if defined(TEST)
static void
test(const char *s)
{
  char *ss = ghc_demangle_sym(strdup(s));
  printf("%s => %s\n", s, ss);
  free(ss);
}

int
main(int argc, char **argv)
{
  test("_ghczmlibzmparserzm0zi20240309zmmAm1qRZZ9s334DFqGWPhD7_GHCziUtilsziPanic_zdfExceptionGhcExceptionzuzdcshow_closure");
  /* test("Trak"); */
  /* test("foozuwib"); */
  /* test("zg1"); */
  /* test("foozhzh"); */
  /* test("foozhzh1"); */
  /* test("ZC"); */
  /* test("ZCzp"); */
  /* test("Z0T"); */
  /* test("Z5H"); */
  /* test("z50U"); */
  return 0;
}
#endif
