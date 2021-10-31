- use `cabal new-build all --allow-newer='ghc-lib-parser:*' --verbose=3` to find the new upper bounds
```
        depends array-0.5.4.0
        depends base-4.16.0.0
        depends binary-0.8.9.0
        depends bytestring-0.11.1.0
        depends containers-0.6.5.1
        depends deepseq-1.4.6.0
        depends directory-1.3.6.2
        depends exceptions-0.10.4
        depends filepath-1.4.2.1
        depends ghc-prim-0.8.0
        depends parsec-3.1.14.0
        depends pretty-1.1.3.6
        depends process-1.6.13.2
        depends time-1.11.1.1
        depends transformers-0.5.6.2
        depends unix-2.7.2.2

```
- adjust `ghc-lib-parser.cabal` and `ghc-lib.cabal` via `commonBuildDepends` in `Ghclibgen.hs` accordingly
