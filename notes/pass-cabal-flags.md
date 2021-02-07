# Configuring Cabal build flags

It's always an emergency when I go looking for this information!

Suppose you are building HLint.

```
mkdir ~/tmp && cd ~/tmp
curl -o hlint-3.2.7.tar.gz https://hackage.haskell.org/package/hlint-3.2.7/hlint-3.2.7.tar.gz
gunzip  hlint-3.2.7.tar.gz && tar xvf  hlint-3.2.7.tar
```

Left to their own devices, HLint and `ghc-lib-parser-ex` will default to `auto` mode meaning, they will decide for themselves if they should depend on `ghc-lib-parser` or native ghc libraries.

Sometimes it's desirable to force the situation though and explicitly make them do one or the other. How you do that? The answer is of course package Cabal flags. There are two scenarios: building with stack or building with cabal.

- `stack.yaml`

  - Force link with `ghc-lib-parser`

    ```
      flags:
        hlint:
          ghc-lib: true
        ghc-lib-parser-ex:
          auto: false
          no-ghc-lib: false
      ```
  - Force link with native ghc

    ```
      flags:
        hlint:
          ghc-lib: false
        ghc-lib-parser-ex:
          auto: false
          no-ghc-lib: true
```
- `cabal.project`

  - Force link with `ghc-lib-parser`

    ```
      packages: hlint-3.2.7
      package hlint
        flags: +ghc-lib
      package ghc-lib-parser-ex
        flags: -auto -no-ghc-lib
    ```
  - Force link with native ghc

    ```
      packages: hlint-3.2.7
      package hlint
        flags: -ghc-lib
      package ghc-lib-parser-ex
        flags: -auto +no-ghc-lib
    ```
