- buck2 is used for most of fbsource
  - hack programming language
  - zoncolan : program security analysis
  - pyre: static typechecker for python
  - flow: static typechecker for javascript
  - superpack: compression software
  - ...
- how many lines of ocaml source? (demonstrated capable of building ocaml at scale)

<!-- the title of an abstract needs to be clear & interesting -->
# the buck2 build tool for ocaml users & developers
buck2 is a large scale build tool developed by & used at meta. buck2 is the successor to buck. buck2 is ready for ocaml users & developers
<!-- In this talk, we'll explore how state machines and statecharts can be used to model even the most complex logic and automatically visualize, generate tests, produce documentation, and more. Learn how to leverage models, save time, and ship faster, more robust React apps. -->
- we'll talk about
  - what is buck2?
  - why is buck2 important?
  - who made it & who uses it
  - what it's good at
  - rules and starlark
  - hey! buck2 knows how to build ocaml!
  - where to get it
  - how to use it
    - examples will be given
      - libraries
      - executables
      - interfacing to opam
      - ocamllex, menhir
      - objects, bytecode & plugins
      - ppx, examining elaborated source
      - interop: embedding & extending with C, Rust
      - "wrapped" libraries
