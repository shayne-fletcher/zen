# Testing a new stack resolver

When there's been a new GHC release, it can take a little while for there to be a stack resolver for it. The following procedure can be used for some local testing if you want to get ahead of the game[^1].

[^1]: *Kindly explained to me by Mike Pilgrem in this [ticket](https://github.com/commercialhaskell/stack/issues/5761). This note is biased towards my needs on macOS. See the linked issue for further details especially for Windows installations.*

## Where stack looks for resolvers

If you execute the command,
```bash
    $stack setup --resolver ghc-x.y.z
```
you'll see something like
```
    No setup information found for ghc-x.y.z on your platform.
```
if there isn't yet a resolver for ghc-x.y.z.

The default set of resolvers `stack` "knows" about are those enumerated in the file [stack-setup-2.yaml]( https://github.com/commercialhaskell/stackage-content/blob/master/stack/stack-setup-2.yaml) in the [commericalhaskell/stackage-content](https://github.com/commercialhaskell/stackage-content) repository.

## Create a local stack setup file

- Start by downloading the release binary package tarball of interest from [www.haskell.org](https://www.haskell.org/ghc/) and note its
  - size (in bytes):
```bash
         stat -f%z ghc-x.y.z-x86_64-apple-darwin.tar.xz
```
  - `SHA1` hash:
```bash
         shasum -a 1 ghc-x.y.z-x86_64-apple-darwin.tar.xz
```
  - `SHA256` hash:
```bash
         shasum -a 256 ghc-x.y.z-x86_64-apple-darwin.tar.xz
```
- Now, create `stack-setup-2-ghc-x.y.z.yaml` with contents along the lines of 
```yaml
        ghc:
          macosx:
            x.y.z:
              url: http://downloads.haskell.org/~ghc/x.y.z/ghc-x.y.z-x86_64-apple-darwin.tar.xz
              content-length: 177152992
              sha1: 2dbd726860ed2c0ea04c7aca29c22df20b952ee1
              sha256: f2e8366fd3754dd9388510792aba2d2abecb1c2f7f1e5555f6065c3c5e2ffec4
```

## Install GHC version `x.y.z` via the setup file

The following `stack setup` command will use the setup file created above to download and install ghc-x.y.z:
```bash
  stack --setup-info-yaml stack-setup-2-ghc-x.y.z.yaml --resolver ghc-x.y.z setup
```

Once `stack` has got GHC installed, there's no further any need to pass a `setup-info-yaml` argument to subsequent `stack` commands, it's ready to go!

