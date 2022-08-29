# `hlint-from-scratch`

Scripts for building `HLint` from GHC `HEAD` (and other flavors).

---

## `hlint-from-scratch-init.sh`

Use `hlint-from-scratch --init=<repo-dir>` to get git clones before using `hlint-from-scratch` to build for the first time.

Other invocations of `hlint-from-scratch` accept a repo-dir argument. If missing `"$HOME"/project` is assumed.

---

## `hlint-from-scratch.sh`

`hlint-from-scratch` orchestrates builds and tests. e.g.

- Full

```bash
    hlint-from-scratch --ghc-flavor=""
```

- Quickest

```bash
    hlint-from-scratch --ghc-flavor="" --no-checkout --no-builds --no-cabal --no-haddock
```

Argument:

  - `--help`

    - print a usage message and quit

  - `--init=<repo-dir>`

    - run `hlint-from-scratch-init.sh` and quit

  - `--ghc-flavor=<ghc-flavor>`
    - either:
      - the empty string i.e. `--ghc-flavor=\"\"` means build `ghc-lib` from GHC `HEAD`
      - or, a `ghc-lib` ghc-flavor: e.g. `ghc-8.8.1`, `ghc-9.2.4` etc.
    - if the selected ghc-flavor is `ghc-master` or `""` (`HEAD`) then the branches of `ghc-lib-parser-ex` & `hlint` (automatically checked out and) used will be their `ghc-next` branches else their `master` branches.

Options:

  - `--repo-dir=<repo-dir>`

    - default: `$HOME/project`
    - directory containing git repository clones

  - `--no-checkout`

    - `ghc-lib` flag: assume a `ghc` checkout exists in the `ghc-lib` project (save time not cloning a new one)
  - `--no-builds`

    - `ghc-lib`, `ghc-lib-parser-ex` flag: skip building and executing anything other than what's necessary to produce sdists
  - `--no-cabal`

    - `hlint-from-scratch` flag: skip execution of `hlint-from-scratch-cbabl-build-and-test.sh`
    - `--no-haddock`

      - `hlint-from-scratch-cabal-build-test` flag: don't generate haddocks
        - *note: haddock generation requires `hlint-from-scratch-cbabl-build-and-test.sh` is not disabled (i.e. option `--no-cabal` is not present)*

  - `--stack-yaml=<stack-yaml>`

    - `ghc-lib` & `ghc-lib-parser-ex` flag: use the stack yaml file of the given name (typically `stack-exact.yaml`)
  - `--resolver=<stack-resolver>`

    - `ghc-lib` && `ghc-lib-parser-ex` flag: use the given stack resolver
    - `hlint` only supports stack curated resolvers right now
      - the current resolver is `nightly-2022-08-04` (ghc-9.2.4)

Example: Start from `ghc-lib` built from a warm GHC master commit. Do the minimum necessary for execution of the `HLint` test-suite. Use a specific compiler (`ghc-9.4.2`) & package set (i.e. determined by `extra-deps` of `stack-exact.yaml`).
```bash
run-head --ghc-flavor="ghc-master" "--no-checkout --no-builds --no-cabal --no-haddock --stack-yaml=stack-exact.yaml --resolver=ghc-9.4.2"
```

---

## `hlint-from-scratch-cabal-build-and-test.sh`

This step harvests the "sdists" from the repo-dir clones produced by the stack build:

 - `ghc-lib-gen-$version.gz`
 - `ghc-lib-parser-$version.gz`
 - `ghc-lib-$version.tar.gz`
 - `ghc-lib-test-utils-$version.tar.gz`
 - `ghc-lib-test-mini-hlint-$version.tar.gz`
 - `ghc-lib-test-mini-compile-$version.tar.gz`
 - `ghc-lib-parser-ex-$version.tar.gz`
 - `hlint-$version.tar.gz`

They are extracted into a temporary directory (root hardcoded to `"$HOME"/tmp/ghc-lib`) named for the `ghc-flavor` and compiler version combination e.g.
```
/Users/shayne/tmp/ghc-lib/0.20220827
❯ tree -L 2
.
└── ghc-9.4.2
    ├── cabal.project
    ├── ghc-lib-0.20220827
    ├── ghc-lib-gen-0.20220827
    ├── ghc-lib-parser-0.20220827
    ├── ghc-lib-parser-ex-0.20220827
    ├── ghc-lib-test-mini-compile-0.20220827
    ├── ghc-lib-test-mini-hlint-0.20220827
    ├── ghc-lib-test-utils-0.20220827
    └── hlint-0.20220827

9 directories, 1 file
```

The `.cabal` files in the collection of packages express constraints on each other's versions. A `cabal.project` file is generated & `cabal new-build all` is invoked. If `--no-haddock` is not provided, `cabal new-haddock all` is invoked. If the build is successful, the `ghc-lib` & `hlint` tests are executed.

---
