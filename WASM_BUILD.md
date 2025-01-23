# Build Project for WebAssembly

The project can be built for [WebAssembly](https://webassembly.org/) in order to be more easily consumed by browsers or a Node.js app.

More specifically, it is also compiled as a [WASI](https://wasi.dev/) module so that the consuming app can leverage I/O and consume the CLI tool using standard output (stdout).

## Prerequisites

Install [Nix](https://nixos.org/) because it's the easiest way to use the [Haskell Wasm tools](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta).

## Build for WASM

1. Launch a ready-made development environment before building using `nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_10' --extra-experimental-features nix-command --extra-experimental-features flakes`, as suggested in [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#getting-started-as-a-nix-flake) docs. Note that we are using the GHC `9.10` flavour, not the latest one.
2. Build for wasm using the following commmand: `wasm32-wasi-cabal build --project-file=cabal.project.wasm-wasi --builddir=dist-wasm`. This leverages the special cabal binary provided in development environment created by Nix and also uses the proper GHC version (`9.10`). It also leverages the `cabal.project.wasm-wasi` overrides that are needed for the project to be built for WebAssembly.
3. The artifact you need is `dist-wasm/build/wasm32-wasi/ghc-9.10.1.20241209/automerge-pandoc-0.1.0.0/x/automerge-pandoc/build/automerge-pandoc/automerge-pandoc.wasm` (or similar). Copy it in the project where you want to leverage the Automerge Pandoc CLI.
4. Exit the Nix development environment by typing `exit`.

## Run using wasmtime

You can confirm that the CLI is working by using the [wasmtime](https://wasmtime.dev/) runtime for WebAssembly. Install `wasmtime` following the instructions in the website and then run:

```
wasmtime automerge-pandoc.wasm fromAutomerge --to pandoc AUTOMERGE_SPANS_JSON
```

## Notes on the Overrides

- The overrides in `cabal.project.wasm-wasi` are based mostly on https://github.com/haskell-wasm/pandoc/blob/wasm/cabal.project, which is a Pandoc build for wasm.
- Pandoc was downgraded to version `3.5` because this was the version that was working with the wasm build at the time of the initial implementation.
- [haskell-wasm](https://github.com/orgs/haskell-wasm/repositories) GitHub repositories is probably the best resource for packages that will work for WASM & WASI.

## More Resources

Haskell's [official docs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) for WebAssembly were the starting point of this implementation and are a useful resource.
