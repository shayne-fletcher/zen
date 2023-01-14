# Cabal package macros (`MIN_VERSION_xyz`)

`cabal build ...` generates `cabal_macros.h` containing e.g. for v 3.5.0 a definition like,
```c
/* package hlint-3.5 */
#ifndef VERSION_hlint
#define VERSION_hlint "3.5"
#endif /* VERSION_hlint */
#ifndef MIN_VERSION_hlint
#define MIN_VERSION_hlint(x,y,z) (\
  (x) <  3 || \
  (x) == 3 && (y) <  5 || \
  (x) == 3 && (y) == 5 && (z) <= 0)
#endif /* MIN_VERSION_hlint */
```
This macro is a compile time predicate. Use to test the `hlint` configured package version is at least `x.y.z`.

We might for example test if the configured package version is at least 3.6.0 by
```c
MIN_VERSION_hlint(3.6.0)
```
By substitution into the macro body we arrive at
```c
3 < 3 || 3 == 3 && 6 < 5 || 3 == 3 and 6 == 5 and 0 <= 0
```
to conclude, no it is not.
