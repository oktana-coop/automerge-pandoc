# v2-automerge-pandoc

CLI tool that converts Automerge spans to Pandoc AST. Built using Haskell.

## Prerequisites

### MacOS

Pandoc needs [pkg-config](https://formulae.brew.sh/formula/pkgconf) to be built in MacOS. To install it, first install [Homebrew](https://brew.sh/) and then run `brew install pkg-config`.

## Notes on Dependency Versions

It wasn't possible to make both [`aeson`](https://hackage.haskell.org/package/aeson) and [VSCode Haskell Extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell#supported-ghc-versions) work with a version newer than version `lts-22.43`, [which corresponds to GHC 9.6.6](https://www.stackage.org/).

This limitation results in using some older versions of `aeson` and `pandoc` (not the latest ones at the time of writing), which is something that needs to be reviewed and ideally solved soon.
