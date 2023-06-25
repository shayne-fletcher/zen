# Wrapped library with masking

Suppose 'wrap' is a library of modules `A` and `B` "packed" into a "root module" `Wrap`. That is, within 'wrap' you write `A.foo` and `B.t` but outside 'wrap' you write `Wrap.A.foo` and `Wrap.B.t` and `A` and `B` are unbound.

In this case all 'wrap' modules are accessible from the root module and this is the simplest example of a wrapped library.

A more general case is one in which not all of a library's modules are accessible through its root module. For reasons of encapsulation and information hiding, such wrapped libraries exhibit masking.

## 'wrap' library with masking example

`Wrap` has this signature
```ocaml
  val print_hello: unit -> unit
  module B: sig val print_hello: unit -> unit end
```
and this implementation.
```ocaml
  let print_hello () = A.print_hello ()

  module B = struct
    include B
   end
```
`A` is implemented as
```
  let print_hello () = B.print_hello ()
```
which like `Wrap` also uses `B`. `B` provides the "real" implementation of `print_hello`.
```
  let print_hello() = Printf.printf "Hello world!\n"
```
In summary, `Wrap` does not admit access to `A` but does provide access to `B` and is representative of the sort of things you see in the wild.
```ocaml
  let _: unit = Wrap.print_hello ()
  let _: unit = Wrap.B.print_hello ()
  (* let _: unit = Wrap.A.print_hello ()*) (* Error: Unbound module Wrap.A *)
  (* let _: unit = B.print_hello ()*) (* Error: Unbound module B *)
```

## Buck2 build scheme for 'wrap'

This section details a Buck2 build procedure for 'wrap', a wrapped library with masking.

Start by renaming `a.mli`, `a.ml`, `b.mli` and `b.ml` to `wrap__A.mli`, `wrap__A.ml`, `wrap__B.mli` and `wrap__B.ml` respectively. Next write `wrap__.mli` and `wrap__.ml` each containing these contents:
```ocaml
  module A = Wrap__A
  module B = Wrap__B
  module Wrap__ = struct end
```

The strategy will be, build a modue map `wrap__`, use it to build an implemetation library `wrap__imp` then, compose `wrap__imp` with a root module to complete the wraped library `wrap`.

The file `al__.mli` will be referenced from both the interface and the implementation so being by exporting it.
```python
  export_file(
      name = "wrap__.mli",
      visibility = [
          ":wrap__",
          ":wrap__imp"
      ],
  )
```
We can use this definition now in the build rule for `wrap__`.
```python
  ocaml_library(
      name = "wrap__",
      compiler_flags = [ "-no-alias-deps" ],
      srcs = [
          ":wrap__.mli",
          "wrap__.ml",
      ],
      visibility = [ ":wrap__imp" ],
  )
```
Both `:wrap__.mli` and `:wrap__` are now used in `wrap__imp`.
```python
  ocaml_library(
      name = "wrap__imp",
      compiler_flags = [
          "-no-alias-deps",
          "-open", "Wrap__"
      ],
      ocamldep_flags = [
          "-open", "Wrap__",
          "-map", "$(location :wrap__.mli)"
      ],
      srcs = glob( [ "wrap__*.ml*" ], exclude = [ "wrap__.ml*" ] ),
      deps = [ ":wrap__" ],
      visibility = [ ":wrap" ],
  )
```
Finally we can assemble `wrap` from `wrap__imp`, `wrap.mli` and `wrap.ml`.
```python
  ocaml_library(
      name = "wrap",
      compiler_flags = [
         "-no-alias-deps",
         "-open", "Wrap__",
      ],
      srcs = glob( [ "wrap.ml*" ] ),
      deps = [ ":wrap__imp" ],
      visibility = [ "PUBLIC" ],
)
```
